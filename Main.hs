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
     filename           :: String
    ,verbose            :: Bool
    ,sortDelay          :: Int
    ,key                :: String
    ,ports              :: [Int]
    ,reorder            :: Bool
    ,ticker             :: Bool
    ,skip               :: Bool         -- skip or wait
    ,drawer             :: MVar Quote   -- transaction drawer
    ,buffer             :: Bool
    ,bufsize            :: Int
    ,endOfStream        :: MVar Bool
    ,finishedPrinting   :: MVar Bool
}

--
getBufferSize :: [Flag] -> Maybe Int
getBufferSize flags = case sizes of
    x:_ -> Just $ abs x
    [] -> Nothing
    where sizes = [size | Buffer size <- flags]

{- main -}
main :: IO ()
main = do
    (optArgs, nonOpts, unrecOpts, errs) <- parseArgs

    -- show help if needed
    if (Help `elem` optArgs) then help
    else do
        -- create skip channel
        emptyDrawer                 <- newEmptyMVar 
        endOfStreamSemaphore        <- newEmptyMVar
        finishedPrintingSemaphore   <- newEmptyMVar
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
        logMsg config $ "option args: " ++ show optArgs
        logMsg config $ "non options: " ++ show nonOpts
        logMsg config $ "unrecognized options" ++ show unrecOpts
        logMsg config $ "errs" ++ show errs
        logMsg config ""

        -- Buffer pointless when not multithreaded
        let errs' = errs ++ if not (skip config) && buffer config
            then ["Buffer option cannot be used without skip channel."]
            else []

        case errs' of
            [] -> do
                {-  Listener loops to read. Forked fun loops for takeMVar. Reorder buffer needs to be in either one.

                    ticker      -> N/A; inside printQuote
                    skip        -> always MVar+thread
                    buffer      -> MVar+thread if skip, otherwise reorder or print in same threaad
                    reorder     -> N/A
                -}
                stream <- ((BSLC.drop 24) <$> (BSLC.readFile $ filename config)) -- open pcap file
                if skip config then do
                    -- Launch a printer thread before the stream starts
                    if buffer config then
                        if reorder config then forkIO $ printerBufferReorder config Q.empty Heap.empty
                        else                   forkIO $ printerBuffer        config Q.empty
                    else
                        if reorder config then forkIO $ printerReorder       config Heap.empty
                        else                   forkIO $ printer              config
                    listenThreaded config stream
                else
                    if reorder config then listenReorder       config stream Heap.empty
                    else                   listen              config stream 
                -- ticker format leaves the cursor at end of line
                if ticker config then putStr "\n" else return ()
                if skip config then do
                    logMsg config "signalling end of stream"
                    putMVar (endOfStream config) True      -- flush printer thread
                    logMsg config "waiting for printer thread to finish"
                    finished <- takeMVar $ finishedPrinting config  -- wait for the printer thread to flush
                    logMsg config "printer appears to have  finished"
                else return ()
            es -> do
                logMsg config "Option error(s):"
                traverse_ (\e -> putStrLn $ "    " ++ e) es

--
parseArgs :: IO ([Flag], [String], [String], [String])
parseArgs = do
    cliArgs <- getArgs
    return $ getOpt' Permute options cliArgs

-- show help
help :: IO ()
help = putStrLn $ usageInfo "\nUsage:\n" options

{- -}
data Flag = Verbose | Help | Skip | Reorder | Ticker | Buffer Int deriving (Show, Eq)  -- Ord

{--}
options :: [OptDescr Flag]
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
listen :: Config -> BSLC.ByteString -> IO ()
listen cfg stream = do
    --putStrLn "listen"
    let mNewQuote = nextQuote cfg stream
    case mNewQuote of
        Just (newQuote, streamRest) -> do
            printQuote cfg newQuote
            listen cfg streamRest
        Nothing -> return ()  -- end of stream

--
listenReorder :: Config -> BSLC.ByteString -> Heap.MinHeap Quote -> IO ()
listenReorder cfg stream reorderBuffer = do
    --putStrLn "listenReorder"
    let mNewQuote = nextQuote cfg stream
    case mNewQuote of
        Just (newQuote, streamRest) -> do
            poppedReorderBuffer <- printOlderThan cfg reorderBuffer (acceptTime newQuote)
            listenReorder cfg streamRest (Heap.insert newQuote poppedReorderBuffer)
        Nothing -> flushReordered cfg reorderBuffer  -- end of stream

--
listenThreaded :: Config -> BSLC.ByteString -> IO ()
listenThreaded cfg stream = do
    --putStrLn "listenThreaded"
    let mNewQuote = nextQuote cfg stream
    case mNewQuote of
        Just (newQuote, streamRest) -> do
            success <- tryPutMVar (drawer cfg) newQuote
            listenThreaded cfg streamRest
        Nothing -> return ()  -- end of stream

-- monitor transaction drawer for new quotes and process them.
-- Because MVars are so slow, this should only be used with a skip channel.
-- quote flow:  stream  -> [drawer] -> [buffer] -> [reorder queue] -> printer
-- signal flushed mvar in config on exit
printer :: Config -> IO ()
printer cfg = do
    --putStrLn "printer"
    mStreamEnded <- tryTakeMVar $ endOfStream cfg
    case mStreamEnded of
        Just True -> do
            logMsg cfg "stream appears to have ended"
            putMVar (finishedPrinting cfg) True
        _ -> do  -- Nothing or Just False
            mNewQuote <- tryTakeMVar $ drawer cfg
            case mNewQuote of
                Nothing -> printer cfg  -- no new quote; loop
                Just newQuote -> do
                    printQuote cfg newQuote  -- no queue, unordered - print immediately
                    printer cfg

-- remember to flush when stream ends
printerBuffer :: Config -> Q.MyQueue Quote -> IO ()
printerBuffer cfg queue = do
    --putStrLn "printerBuffer"
    mStreamEnded <- tryTakeMVar $ endOfStream cfg
    case mStreamEnded of
        Just True -> do
            logMsg cfg "stream appears to have ended."
            flushBuffer cfg queue
            putMVar (finishedPrinting cfg) True
        _ -> do  -- Nothing or Just False
            mNewQuote <- tryTakeMVar $ drawer cfg
            case mNewQuote of
                Just newQuote -> do
                    logMsg cfg $ "queue size = " ++ (show $ Q.size queue)
                    if Q.size queue == bufsize cfg then do  -- queue full, so skip new quote
                        let (poppedQueue, mQuote) = Q.pop queue
                        case mQuote of
                            Just quote -> do
                                printQuote cfg quote
                                printerBuffer cfg poppedQueue
                            Nothing -> error "full queue has no items"
                    else
                        if Q.size queue < bufsize cfg then printerBuffer cfg (Q.push newQuote queue)  -- quickly add to queue without printing anything
                        else error "queue exceeded limit"  -- In production this should pop+print from the buffer
                Nothing -> do  -- no new quote, so pop one from buffer
                    let (poppedQueue, mQuote) = Q.pop queue
                    case mQuote of
                        Nothing -> printerBuffer cfg queue  -- nothing in queue
                        Just quote -> do
                            printQuote cfg quote
                            printerBuffer cfg poppedQueue

--
printerReorder :: Config -> Heap.MinHeap Quote -> IO ()
printerReorder cfg reorderBuffer = do
    --putStrLn "printerReorder"
    mStreamEnded <- tryTakeMVar $ endOfStream cfg
    case mStreamEnded of
        Just True -> do
            logMsg cfg "stream appears to have ended."
            flushReordered cfg reorderBuffer
            putMVar (finishedPrinting cfg) True
        _ -> do  -- Nothing or Just False
            -- print any quotes that are old enough
            mNewQuote <- tryTakeMVar $ drawer cfg
            case mNewQuote of
                Just newQuote -> do
                    newReorderBuffer <- printOlderThan cfg reorderBuffer (acceptTime newQuote)
                    printerReorder cfg (Heap.insert newQuote newReorderBuffer)
                Nothing -> printerReorder cfg reorderBuffer

--
printerBufferReorder :: Config -> Q.MyQueue Quote -> Heap.MinHeap Quote -> IO ()
printerBufferReorder cfg queue reorderBuffer = do
    --putStrLn "printerBufferReorder"
    mStreamEnded <- tryTakeMVar $ endOfStream cfg
    case mStreamEnded of
        Just True -> do
            logMsg cfg "stream appears to have ended."
            if Q.size queue > 0 then do  -- move all from queue into reorderBuffer before flushing
                logMsg cfg "transferring queue"
                finalReorderedBuffer <- transferBuffer cfg queue reorderBuffer
                flushReordered cfg finalReorderedBuffer
            else do
                flushReordered cfg reorderBuffer
            putMVar (finishedPrinting cfg) True
        _ -> do  -- Nothing or Just False
            mNewQuote <- tryTakeMVar $ drawer cfg
            case mNewQuote of
                Just newQuote -> do
                    logMsg cfg $ "queue size = " ++ (show $ Q.size queue)
                    if Q.size queue == bufsize cfg then do  -- queue full, so skip new quote
                        let (poppedQueue, mQuote) = Q.pop queue
                        case mQuote of
                            Just quote -> do  -- insert popped quote into reorder buffer
                                newReorderBuffer <- printOlderThan cfg reorderBuffer (acceptTime quote)
                                printerBufferReorder cfg poppedQueue newReorderBuffer
                            Nothing -> error "full queue has no items"
                    else
                        -- quickly add to queue without printing anything
                        if Q.size queue < bufsize cfg then printerBufferReorder cfg (Q.push newQuote queue) reorderBuffer  
                        -- In production this should pop+print from the buffer to drop down to size limit
                        else error "queue exceeded limit"  
                Nothing -> do  -- no new quote, so pop one from buffer
                    let (poppedQueue, mQuote) = Q.pop queue
                    case mQuote of
                        Just quote -> do  -- insert popped quote into reorder buffer
                            newReorderBuffer <- printOlderThan cfg reorderBuffer (acceptTime quote)
                            printerBufferReorder cfg poppedQueue (Heap.insert quote newReorderBuffer)
                        -- no new quote, nothing in queue
                        Nothing -> printerBufferReorder cfg queue reorderBuffer  

-- read the next UDP packet from a pcap
-- Returns Nothing when stream ends
nextQuote :: Config -> BSLC.ByteString -> Maybe (Quote, BSLC.ByteString)
nextQuote cfg stream = 
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
            if BSLC.unpack keyVal == key cfg
            then Just (packetToQuote packetTime packetQuote, rest4)
            else nextQuote cfg $ BSLC.drop pcapLength rest -- rest used twice !!!!!!!!!1
        else nextQuote cfg  $ BSLC.drop pcapLength rest
    else Nothing

-- see if there's a quote in the drawer
nextQuoteDrawer :: Config -> IO (Maybe Quote)
nextQuoteDrawer cfg = tryTakeMVar $ drawer cfg                    

--
logMsg :: Config -> String -> IO ()
logMsg cfg s = if verbose cfg then putStrLn s else return ()

-- Result is the modified heap
printOlderThan :: Config -> Heap.MinHeap Quote -> Int -> IO (Heap.MinHeap Quote)
printOlderThan cfg reorderBuf newAcceptTime =
    case Heap.view reorderBuf of
        Nothing -> return reorderBuf
        Just (oldestQuote, poppedBuf) -> do
            if newAcceptTime - (acceptTime oldestQuote) > (sortDelay cfg) then do
                printQuote cfg oldestQuote
                printOlderThan cfg poppedBuf newAcceptTime
            else return reorderBuf

-- transfers quotes from queue to reorder buffer, while printing from reorder buffer
transferBuffer :: Config -> Q.MyQueue Quote -> Heap.MinHeap Quote -> IO (Heap.MinHeap Quote)
transferBuffer c q rb = do
    --putStrLn "trasnferBuffer"
    let (poppedQueue, mQuote) = Q.pop q
    case mQuote of
        Just quote -> do  -- insert popped quote into reorder buffer
            newRB <- printOlderThan c rb (acceptTime quote)
            transferBuffer c poppedQueue newRB
        Nothing -> return rb

--
flushBuffer :: Config -> Q.MyQueue Quote -> IO ()
flushBuffer cfg queue = do
    let (poppedBuf, mQuote) = Q.pop queue
    case mQuote of
        Just q -> do
            printQuote cfg q
            flushBuffer cfg poppedBuf
        Nothing -> return ()

--
flushReordered :: Config -> Heap.MinHeap Quote -> IO ()
flushReordered cfg buf = do
    case Heap.view buf of
        Just (oldestQuote, poppedBuf) -> do
            printQuote cfg oldestQuote
            flushReordered cfg poppedBuf
        Nothing -> return ()

{- Leading pad with zeros. 
    `n` is the target length.
    If `s` has length more than `n`, just return `s`.
-}
padWithZeros :: Int -> String -> String
padWithZeros n s
    | len >= n = s
    | otherwise = replicate (n - len) '0' ++ s
    where len = length s

{- Accept packet time and the 210 bytes after B6034 and turn them into a quote -}
packetToQuote :: String -> BSLC.ByteString -> Quote
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
printQuote :: Config -> Quote -> IO ()
printQuote cfg quote = do
    if ticker cfg then do
        setCursorColumn 0
        putStr $ show quote
    else
        putStrLn $ show quote

{- Break a byte string up into arbitrarily sized substrings. -}
split :: BSLC.ByteString -> [Int64] -> [BSLC.ByteString]
split _ [] = []
split bs (size:sizes) = BSLC.take size bs : split (BSLC.drop size bs) sizes

{- Quote data type -}
data Quote = Quote
    {                      
      packetTime :: String
    , issueCode  :: String 
    , b1         :: String 
    , b2         :: String 
    , b3         :: String 
    , b4         :: String 
    , b5         :: String 
    , a1         :: String    
    , a2         :: String
    , a3         :: String
    , a4         :: String
    , a5         :: String
    , acceptTime :: Int
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


