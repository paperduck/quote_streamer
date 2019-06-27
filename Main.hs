--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC -fno-omit-yields #-}

{-
TODO
    haddock comments
    currying
    readerT for args
    rest of options
    make sure buffer size positive
    rest used twice
    thread safe (protect MVars via semaphore?)
    explain what a transaction drawer is
    "A single MVar is used to separate... disjoint
    buffer is sort of a debounce
    test cases
    verbose output
    ReaderT is mainly used to demonstrate ability. Can't use in forked thread?
-}

{- README
    -- quote pipeline:  stream  -> drawer -> buffer -> reorder queue -> printer
-}

module Main where

import           Data.Foldable(traverse_)
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception
import qualified Data.ByteString.Lazy.Char8         as BSLC
import qualified Data.Heap                          as Heap
import           Data.Int (Int64)
--import           Data.Map.Strict (Map, fromList, lookup)
import           Network.Transport.Internal (decodeNum16, decodeNum32)
import           System.Console.GetOpt
import           System.Console.ANSI
import           System.Environment
import           Control.Monad.Reader

import qualified MyQueue as Q

--data EndOfStreamException = EndOfStreamException e
--instance Exception EndOfStreamException

data Config = Config {
     filename           :: !String
    ,verbose            :: !Bool
    ,sortDelay          :: !Int
    ,key                :: !String
    ,ports              :: ![Int]
    ,reorder            :: !Bool
    ,ticker             :: !Bool
    ,skip               :: !Bool         -- skip or wait
    ,drawer             :: MVar (Maybe Quote)   -- transaction drawer; Nothing = stream ends
    ,buffer             :: !Bool
    ,bufsize            :: !Int
    ,endOfStream        :: MVar Bool
    ,finishedPrinting   :: MVar Bool
}

--
getBufferSize :: [Flag] -> Maybe Int
{-# INLINE getBufferSize #-}
getBufferSize flags = case sizes of
    x:_ -> Just $ abs x
    [] -> Nothing
    where sizes = [size | Buffer size <- flags]

{- main -}
main :: IO ()
main = do
    (optArgs, nonOpts, unrecOpts, errs) <- parseArgs

    -- show help if needed
    if (Help `elem` optArgs) then showHelp
    else do
        -- create skip channel
        emptyDrawer                 <- newEmptyMVar 
        finishedPrintingSemaphore   <- newEmptyMVar
        endOfStreamSemaphore        <- newEmptyMVar
        let bufferSize = getBufferSize optArgs
        --putStrLn $ "bufzise = " ++ (show bufferSize)
        let config = Config {
            filename="mdf-kospi200.20110216-0.pcap" -- PCAP file
            ,verbose=(Verbose `elem` optArgs)
            ,sortDelay=300                          -- quote/packet accept time threshold, in hundredths of a second
            ,key="B6034"                            -- target  Data Type + Information Type + Market Type
            ,ports=[15515, 15516]
            ,reorder=(Reorder `elem` optArgs)
            ,ticker=(Ticker `elem` optArgs)
            ,skip=(Skip `elem` optArgs)
            ,drawer=emptyDrawer
            ,buffer=case getBufferSize optArgs of
                Just x -> True
                Nothing -> False
            ,bufsize=case getBufferSize optArgs of
                Just x -> x
                Nothing -> 0
            ,endOfStream=endOfStreamSemaphore
            ,finishedPrinting=finishedPrintingSemaphore
        }
        runReaderT (logMsg $ "option args: " ++ show optArgs) config
        runReaderT (logMsg $ "non options: " ++ show nonOpts) config
        runReaderT (logMsg $ "unrecognized options" ++ show unrecOpts) config
        runReaderT (logMsg $ "errs" ++ show errs) config
        runReaderT (logMsg "") config

        -- Buffer pointless when not multithreaded
        let errs' = errs ++ if not (skip config) && buffer config
            then ["Buffer option cannot be used without skip channel."]
            else []

        case errs' of
            [] -> do
                stream <- ((BSLC.drop 24) <$> (BSLC.readFile $ filename config)) -- open pcap file
                if skip config then do
                    -- Launch a printer thread before the stream starts
                    if buffer config then
                        if reorder config then forkIO $ runReaderT (printerBufferReorder Q.empty Heap.empty) config
                        else                   forkIO $ runReaderT (printerBuffer        Q.empty)            config
                    else
                        if reorder config then forkIO $ runReaderT (printerReorder               Heap.empty) config
                        else                   forkIO $ runReaderT (printer                                ) config
                    runReaderT (listenThreaded stream) config
                else
                    if reorder config then runReaderT (listenReorder stream Heap.empty) config
                    else                   runReaderT (listen        stream)            config
                if skip config then do
                    runReaderT (logMsg "\nsignalling end of stream") config
                    putMVar (drawer config) Nothing      -- signal to flush printer thread
                    runReaderT (logMsg "waiting for printer thread to finish") config 
                    finished <- takeMVar $ finishedPrinting config  -- wait for the printer thread to flush
                    runReaderT (logMsg "printer appears to have  finished") config
                else return ()
                if ticker config then putStr "\n" else return ()  -- ticker leaves cursor at end of line
            es -> do
                runReaderT (logMsg "Option error(s):") config
                traverse_ (\e -> putStrLn $ "    " ++ e) es

parseArgs = do
    cliArgs <- getArgs
    return $ getOpt' Permute options cliArgs

-- show help
showHelp :: IO ()
{-# INLINE showHelp #-}
showHelp = putStrLn $ usageInfo "\nUsage:\n" options

{- -}
data Flag = Verbose | Help | Skip | Reorder | Ticker | Buffer Int deriving (Show, Eq)  -- Ord

{--}
options :: [OptDescr Flag]
{-# INLINE options #-}
options = [ 
     Option ['v'] ["verbose"]      (NoArg Verbose)  "Verbose output."
    ,Option ['h'] ["help"]         (NoArg Help)      "Show help."
    ,Option ['r'] ["reorder"]      (NoArg Reorder)   "Reorder quotes by quote accept time."
    ,Option ['t'] ["ticker"]       (NoArg Ticker)    "Quotes display on one line, overwriting the previous quote."
    ,Option ['s'] ["skip-channel"] (NoArg Skip)      "Use a skip channel. Skips quotes in order to eliminate bottleneck when printing."
    ,Option ['b'] ["buffer"]       
        (ReqArg (\s -> (Buffer (read s::Int))) "BUFSIZE" )
        "Buffer up to n quotes to be printed. Useful if stream is intermittent and you want to minimize skipped data."
    ]

-- no skip, no queue, no reorder
listen :: BSLC.ByteString -> ReaderT Config IO ()
listen stream = do
    --liftIO $ putStrLn "listen"
    cfg <- ask
    case nextQuote (key cfg) stream of
        Just (newQuote, streamRest) -> do
            printQuote newQuote
            listen streamRest
        Nothing -> return ()  -- end of stream

--
listenReorder :: BSLC.ByteString -> Heap.MinHeap Quote -> ReaderT Config IO ()
listenReorder stream reorderBuffer = do
    --liftIO $ putStrLn "listenReorder"
    cfg <- ask
    case nextQuote (key cfg) stream of
        Just (newQuote, streamRest) -> do
            poppedReorderBuffer <- printOlderThan reorderBuffer (acceptTime newQuote)
            listenReorder streamRest (Heap.insert newQuote poppedReorderBuffer)
        Nothing -> flushReordered reorderBuffer  -- end of stream

--
listenThreaded :: BSLC.ByteString -> ReaderT Config IO ()
listenThreaded stream = do
    --liftIO $ putStrLn "listenThreaded"
    cfg <- ask
    case nextQuote (key cfg) stream of
        Just (newQuote, streamRest) -> do
            _ <- liftIO $ tryPutMVar (drawer cfg) (Just newQuote)
            listenThreaded streamRest
        Nothing -> return ()  -- end of stream

-- monitor transaction drawer for new quotes and process them.
-- Because MVars are so slow, this should only be used in conjunction with a skip channel
printer :: ReaderT Config IO ()
printer = do
    --liftIO $ putStrLn "printer"
    cfg <- ask
    mNewQuote <- liftIO . takeMVar $ drawer cfg
    case mNewQuote of
        Nothing -> do
            logMsg "\nstream appears to have ended"
            liftIO $ putMVar (finishedPrinting cfg) True
        Just newQuote -> printQuote newQuote >> printer

-- tryTake  
--          -> Just mNewQ   
--                        (1)  -> Just newQ     -> buffer is  full  -> print 1 from buffer  -> recurse
--                                              -> buffer not full  -> push to buffer -> recurse
--                        (2)  -> Just Nothing  -> end of stream
--          -> Nothing      
--                          -> buffer empty     -> take (block) 
--                                                              -> (1)
--                                                              -> (2)
--                          -> buffer not empty -> print 1 from buffer  -> recurse
printerBuffer :: Q.MyQueue Quote -> ReaderT Config IO ()
printerBuffer queue = do
    --liftIO $ putStrLn "printerBuffer"
    cfg <- ask
    --logMsg $ "queue size = " ++ (show $ Q.size queue)
    mmNewQuote <- liftIO . tryTakeMVar $ drawer cfg
    case mmNewQuote of
        Just mNewQuote -> handleMNewQuote mNewQuote
        Nothing -> do 
            -- No new quote, so use this opportunity to pop the buffer
            let (poppedQueue, mQuote) = Q.pop queue
            case mQuote of
                -- buffer is empty, so we can block
                Nothing -> do
                    mNewQuote <- liftIO . takeMVar $ drawer cfg
                    handleMNewQuote mNewQuote  
                -- pop from buffer
                Just quote -> printQuote quote >> printerBuffer poppedQueue
        where
            handleMNewQuote mNewQuote = do
                cfg <- ask
                case mNewQuote of
                    Just newQuote -> if Q.size queue >= bufsize cfg
                         -- queue full; skip new quote
                        then do 
                            let (poppedQueue, mQuote) = Q.pop queue
                            case mQuote of
                                Just quote -> do
                                    printQuote quote
                                    printerBuffer poppedQueue
                                Nothing -> error "full queue has no items"

                        else printerBuffer (Q.push newQuote queue)  -- quickly add to queue without printing anything
                    Nothing -> do  -- end of stream
                        logMsg "stream appears to have ended."
                        flushBuffer queue
                        liftIO $ putMVar (finishedPrinting cfg) True  -- !!!! prevent deadlock?

--
printerReorder :: Heap.MinHeap Quote -> ReaderT Config IO ()
printerReorder reorderBuffer = do
    --liftIO $ putStrLn "printerReorder"
    cfg <- ask
    mNewQuote <- liftIO . takeMVar $ drawer cfg
    case mNewQuote of
        -- print any quotes that are old enough
        Just newQuote -> do
            newReorderBuffer <- printOlderThan reorderBuffer (acceptTime newQuote)
            printerReorder (Heap.insert newQuote newReorderBuffer)
        Nothing -> do
            logMsg "stream appears to have ended."
            flushReordered reorderBuffer
            liftIO $ putMVar (finishedPrinting cfg) True  -- ensure this doesn't deadlock   !!!!!

--
printerBufferReorder :: Q.MyQueue Quote -> Heap.MinHeap Quote -> ReaderT Config IO ()
printerBufferReorder queue reorderBuffer = do
    --liftIO $ putStrLn "printerBufferReorder"
    cfg <- ask
    --logMsg $ "queue size = " ++ (show $ Q.size queue)
    mmNewQuote <- liftIO . tryTakeMVar $ drawer cfg 
    case mmNewQuote of
        Just mNewQuote -> handleMNewQuote mNewQuote
        Nothing -> do
            -- No new quote, so use this opportunity to pop one from the buffer
            let (poppedQueue, mQuote) = Q.pop queue
            case mQuote of
                -- Queue empty. Nice! We can block until a new quote arrives.
                Nothing -> (liftIO . takeMVar $ drawer cfg) >>= handleMNewQuote
                -- Transfer quote from buffer (queue) to reorder buffer
                Just quote -> do  
                    newReorderBuffer <- printOlderThan reorderBuffer (acceptTime quote)
                    printerBufferReorder poppedQueue (Heap.insert quote newReorderBuffer)
        where 
            handleMNewQuote mNewQuote = do
                cfg <- ask
                case mNewQuote of
                    Just newQuote -> do
                        -- try to put new quote onto buffer 
                        if Q.size queue >= bufsize cfg then do  
                            -- buffer full, so skip this quote and pop one from the buffer
                            let (poppedQueue, mQuote) = Q.pop queue
                            case mQuote of
                                -- insert popped quote into reorder buffer
                                Just quote ->  do
                                    newReorderBuffer <- printOlderThan reorderBuffer (acceptTime quote)
                                    printerBufferReorder poppedQueue newReorderBuffer
                                Nothing -> error "full queue has no items    2"
                        else printerBufferReorder (Q.push newQuote queue) reorderBuffer  
                    Nothing -> do
                        logMsg "stream appears to have ended."
                        -- move all from queue into reorderBuffer before flushing
                        if Q.size queue > 0 then do
                            logMsg "transferring queue"
                            transferBuffer queue reorderBuffer >>= flushReordered
                        else
                            flushReordered reorderBuffer
                        liftIO $ putMVar  (finishedPrinting cfg) True

-- read the next UDP packet from a pcap
-- Returns Nothing when stream ends
nextQuote :: String -> BSLC.ByteString -> Maybe (Quote, BSLC.ByteString)
nextQuote key stream = 
    if not (BSLC.null stream) then do
        -- extract PCAP header
        let (pcapHeader, rest)              = BSLC.splitAt 16 stream
        let (bsPacketTimeSec,  pcapRest1)   = BSLC.splitAt 4 pcapHeader                -- packet time, sec
        let (bsPacketTimeUSec, pcapRest2)   = BSLC.splitAt 4 pcapRest1                 -- packet time, usec
        let bsPcapLength                    = BSLC.take 4 $ BSLC.drop 4 pcapRest2      -- packet length      
        let packetTime = (show $ decodeNum32 $ BSLC.toStrict $ BSLC.reverse $ bsPacketTimeSec)
             ++ (padWithZeros 6 $ show $ decodeNum32 $ BSLC.toStrict $ BSLC.reverse bsPacketTimeUSec)
        let pcapLength = decodeNum32 $ BSLC.toStrict $ BSLC.reverse bsPcapLength
        -- extract UDP header 
        -- 14 ethernet header + 20 IPv4 header + 8 UDP header (4 ports + 4 ignore)
        let (bsDestPort,        rest2)      = BSLC.splitAt 2 $ BSLC.drop 36 rest
        let intDestPort                     = decodeNum16 $ BSLC.toStrict bsDestPort
        -- Check port
        if intDestPort `elem` [15515, 15516] then do
            let (keyVal,        rest3)      = BSLC.splitAt 5 $ BSLC.drop 4 rest2
            let (packetQuote,   rest4)      = BSLC.splitAt 210 rest3
            if BSLC.unpack keyVal == key
            then Just (packetToQuote packetTime packetQuote, rest4)
            else nextQuote key $ BSLC.drop pcapLength rest -- rest used twice !!!!!!!!!1
        else nextQuote key $ BSLC.drop pcapLength rest
    else Nothing

--
logMsg :: String -> ReaderT Config IO ()
{-# INLINE logMsg #-}
logMsg msg = asks verbose >>= (\v -> if v then liftIO . putStrLn $ msg else return ())

-- Result is the modified heap
printOlderThan :: Heap.MinHeap Quote -> Int -> ReaderT Config IO (Heap.MinHeap Quote)
printOlderThan reorderBuf newAcceptTime = do
    cfg <- ask
    case Heap.view reorderBuf of
        Nothing -> return reorderBuf
        Just (oldestQuote, poppedBuf) -> do
            if newAcceptTime - (acceptTime oldestQuote) > (sortDelay cfg) then do
                printQuote oldestQuote
                printOlderThan poppedBuf newAcceptTime
            else return reorderBuf

-- transfers quotes from queue to reorder buffer, while printing from reorder buffer
transferBuffer :: Q.MyQueue Quote -> Heap.MinHeap Quote -> ReaderT Config IO (Heap.MinHeap Quote)
transferBuffer q rb = do
    --putStrLn "trasnferBuffer"
    cfg <- ask
    let (poppedQueue, mQuote) = Q.pop q
    case mQuote of
        Just quote -> do  -- insert popped quote into reorder buffer
            newRB <- printOlderThan rb (acceptTime quote)
            transferBuffer poppedQueue newRB
        Nothing -> return rb

-- Print quotes remaining in buffer
flushBuffer :: Q.MyQueue Quote -> ReaderT Config IO ()
flushBuffer queue = do
    cfg <- ask
    let (poppedBuf, mQuote) = Q.pop queue
    case mQuote of
        Just q -> do
            printQuote q
            flushBuffer poppedBuf
        Nothing -> return ()  -- finished

-- Print remaining reordered quotes 
flushReordered :: Heap.MinHeap Quote -> ReaderT Config IO ()
flushReordered buf = do
    cfg <- ask
    case Heap.view buf of
        Just (oldestQuote, poppedBuf) -> do
            printQuote oldestQuote
            flushReordered poppedBuf
        Nothing -> return ()  -- finished

{- Leading pad with zeros. 
    `n` is the target length.
    If `s` matches or exceeds target length, return `s` as-is.
-}
padWithZeros :: Int -> String -> String
{-# INLINE padWithZeros #-}
padWithZeros n s
    | len >= n = s
    | otherwise = replicate (n - len) '0' ++ s
    where len = length s

{- Accept packet time and the 210 bytes after B6034 and turn them into a quote -}
packetToQuote :: String -> BSLC.ByteString -> Quote
{-# INLINE packetToQuote #-}
packetToQuote packetTime bs = Quote packetTime issueCode b1 b2 b3 b4 b5 a1 a2 a3 a4 a5 acceptTime
   where
       items = map BSLC.unpack $ split (bs) [12,12, 5,7,5,7,5,7,5,7,5,7, 7, 5,7,5,7,5,7,5,7,5,7, 50, 8, 1]
       issueCode = items!!0
       b1 = items!!3  ++ "@" ++ items!!2 -- bid 1: qty@price
       b2 = items!!5  ++ "@" ++ items!!4
       b3 = items!!7  ++ "@" ++ items!!6
       b4 = items!!9  ++ "@" ++ items!!8
       b5 = items!!11 ++ "@" ++ items!!10
       a1 = items!!14 ++ "@" ++ items!!13
       a2 = items!!16 ++ "@" ++ items!!15
       a3 = items!!18 ++ "@" ++ items!!17
       a4 = items!!20 ++ "@" ++ items!!19
       a5 = items!!22 ++ "@" ++ items!!21
       acceptTime = read (items!!24)::Int

{- Prints a Quote. -}
printQuote :: Quote -> ReaderT Config IO ()
{-# INLINE printQuote #-}
printQuote quote = do
    cfg <- ask
    if ticker cfg then liftIO $ do
        setCursorColumn 0
        putStr $ ( show quote )
    else
        liftIO . putStrLn $ show quote

{- Break a byte string up into arbitrarily sized substrings. -}
split :: BSLC.ByteString -> [Int64] -> [BSLC.ByteString]
{-# INLINE split #-}
split _ [] = []
split bs (size:sizes) = BSLC.take size bs : split (BSLC.drop size bs) sizes

{- Quote data type -}
data Quote = Quote
    {                      
      packetTime :: !String
    , issueCode  :: !String 
    , b1         :: !String 
    , b2         :: !String 
    , b3         :: !String 
    , b4         :: !String 
    , b5         :: !String 
    , a1         :: !String    
    , a2         :: !String
    , a3         :: !String
    , a4         :: !String
    , a5         :: !String
    , acceptTime :: !Int
    } deriving (Eq)

{- `Show` instance for `Quote` -}
instance Show Quote where
    show (Quote packetTime issueCode b1 b2 b3 b4 b5 a1 a2 a3 a4 a5 acceptTime) = packetTime
        ++ " " ++ show acceptTime
        ++ " " ++ issueCode
        ++ " " ++ b5
        ++ " " ++ b4
        ++ " " ++ b3
        ++ " " ++ b2
        ++ " " ++ b1
        ++ " " ++ a1
        ++ " " ++ a2
        ++ " " ++ a3
        ++ " " ++ a4
        ++ " " ++ a5

{- `Ord` instance for `Quote` -}
instance Ord Quote where
    compare q1 q2 = compare (acceptTime q1, packetTime q1) (acceptTime q2, packetTime q2)


