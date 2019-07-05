
{-
TODO
    test cases
    check for duplicate packets
    change packetTime to one big int to avoid trouble with string length
    undersatnd splitAts error cases
-}

{- UPDATES SINCE v.1
    Removed padWithZeros
    Use * instead of decodeNum32 
        - decodeNum32 uses unsafeIO
        - it's nice to have one less dependancy
    add command line options
        - multithreading / skip channel
        - buffer
        - ticker
        - verbosity
    put environment into ReaderT
    added INLINE pragmas
    Added GHC option to increase compiler optmizations
    Convert split function to splitAts
    Added strictness markers (!) where possible
    Make code more compact/elegant
-}

{- README
    UDP parsing still very rudimentary
    quote pipeline:  stream  -> drawer -> buffer -> reorder queue -> printer
    explain what a transaction drawer is
    "A single MVar is used to create a skip channel, eliminating bottleneck"
    buffer is sort of a debounce
    
    KNOWN BUGS
    If your terminal is not wide enough to fit the whole quote on one line,
         the ticker option (-t) may not work as expected.
-}

module Main where

import           Control.Concurrent         (forkIO)
import           Control.Concurrent.MVar    (takeMVar, putMVar, tryTakeMVar, tryPutMVar, newEmptyMVar)
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Heap                  as Heap
import           Data.Int                   (Int64)
import           Data.Maybe                 (maybeToList)
--import           Network.Transport.Internal (decodeNum32, decodeNum16)
import           System.Console.GetOpt      (getOpt', OptDescr(Option), ArgDescr(ReqArg, NoArg), usageInfo, ArgOrder(Permute))
import           System.Environment         (getArgs)
import           Control.Monad.Reader       (runReaderT, ReaderT, ask, liftIO, liftM)

import           Model                      (Config(..), Flag(..), Quote(..))
import           View                       (logMsg, printQuote, showErrors)
import qualified MyQueue                    as Q

-- Parse out the buffer size, if it exists, from the option flags
getBufferSize :: [Flag] -> Maybe Int
{-# INLINE getBufferSize #-}
getBufferSize flags = case sizes of
    x:_ -> Just x
    [] -> Nothing
    where sizes = [size | Buffer size <- flags]

{- main -}
main :: IO ()
main = do
    -- Gather command line options
    (optArgs, nonOpts, unrecOpts, cmdErrs)  <- liftM (getOpt' Permute options) getArgs

    -- environment settings
    emptyDrawer                 <- newEmptyMVar 
    finishedPrintingSemaphore   <- newEmptyMVar
    endOfStreamSemaphore        <- newEmptyMVar
    let bufferSize  = getBufferSize optArgs
    let config      = Config {
        filename            ="mdf-kospi200.20110216-0.pcap"
        ,verbose            =(Verbose `elem` optArgs)
        ,sortDelay          =300      -- accept time threshold in 1/100 seconds 
        ,key                ="B6034"  -- target  Data Type + Information Type + Market Type
        ,ports              =[15515, 15516]
        ,reorder            =(Reorder `elem` optArgs)
        ,ticker             =(Ticker `elem` optArgs)
        ,skip               =(Skip `elem` optArgs)
        ,drawer             =emptyDrawer
        ,buffer             =case getBufferSize optArgs of
                                Just x -> True
                                Nothing -> False
        ,bufsize            =case getBufferSize optArgs of
                                Just x -> x
                                Nothing -> 0
        ,endOfStream        =endOfStreamSemaphore
        ,finishedPrinting   =finishedPrintingSemaphore
    }

    -- Show command line options
    runReaderT (logMsg $ "\nOption arguments: " ++ show optArgs
        ++ "\nNon options: "            ++ show nonOpts
        ++ "\nUnrecognized options: "   ++ show unrecOpts
        ++ "\nOption erorrs: "          ++ show cmdErrs    ) config
    
    -- Gather all error messages
    let errorMsgs = cmdErrs
            -- unrecognized options
            ++ (map ("Unrecognized option: " ++) unrecOpts)
            -- bufsize must be > 0
            ++ if (buffer config) && (bufsize config <= 0)
                then ["Buffer size must be greater than zero."]
                else []
            -- Buffer pointless when not multithreaded
            ++ if not (skip config) && buffer config
                then ["Buffer option cannot be used without skip channel."]
                else []
    case errorMsgs of
        [] -> if (Help `elem` optArgs)  -- show help
            then putStrLn $ usageInfo "\nUsage:\n" options
            else do
                -- open pcap file and drop pcap header
                stream <- liftM (BSLC.drop 24) (BSLC.readFile $ filename config)
                if skip config then do
                    -- Launch a printer thread before the stream starts
                    if buffer config then
                        if reorder config then forkIO $ runReaderT (printerBufferReorder Q.empty Heap.empty) config
                        else                   forkIO $ runReaderT (printerBuffer        Q.empty)            config
                    else
                        if reorder config then forkIO $ runReaderT (printerReorder               Heap.empty) config
                        else                   forkIO $ runReaderT (printer                                ) config
                    -- start stream
                    runReaderT (listenThreaded stream) config
                else
                    if reorder config then runReaderT (listenReorder stream Heap.empty) config
                    else                   runReaderT (listen        stream)            config
                -- Once the stream ends, wait for the printer thread to terminate
                if skip config then do
                    runReaderT (logMsg "\nSignalling end of stream.") config
                    putMVar (drawer config) Nothing  -- signal to thread
                    runReaderT (logMsg "\nWaiting for printer thread to finish.") config 
                    finished <- takeMVar $ finishedPrinting config  -- block
                    runReaderT (logMsg "\nPrinter thread appears to have finished.") config
                else return ()
                -- ticker leaves cursor at end of line
                if ticker config then putStr "\n" else return ()
        es -> showErrors es

{--}
options :: [OptDescr Flag]
{-# INLINE options #-}
options = [ 
     Option ['v'] ["verbose"] (NoArg Verbose) 
        "Verbose output"
    ,Option ['h'] ["help"] (NoArg Help) 
        "Show help"
    ,Option ['r'] ["reorder"] (NoArg Reorder) 
        "Reorder by quote accept time"
    ,Option ['t'] ["ticker"] (NoArg Ticker) 
        "Each quote overwrites the previous one"
    ,Option ['s'] ["skip-channel"] (NoArg Skip) 
        "Skip quotes to eliminate printing bottleneck."
    ,Option ['b'] ["buffer"] (ReqArg (\s -> Buffer (read s::Int)) "BUFSIZE")
        "Buffer up to n quotes to be printed. Not as useful as it seems."
    ]

-- no skip, no queue, no reorder
listen :: BSLC.ByteString -> ReaderT Config IO ()
listen stream = do
    cfg <- ask
    mq <- nextQuote stream
    case mq of
        Just (newQuote, streamRest) -> do
            printQuote newQuote
            listen streamRest
        Nothing -> return ()  -- end of stream

--
listenReorder :: BSLC.ByteString -> Heap.MinHeap Quote -> ReaderT Config IO ()
listenReorder stream reorderBuffer = do
    cfg <- ask
    mq <- nextQuote stream
    case mq of
        Just (newQuote, streamRest) -> do
            poppedReorderBuffer <- printOlderThan reorderBuffer (acceptTime newQuote)
            listenReorder streamRest (Heap.insert newQuote poppedReorderBuffer)
        Nothing -> flushReordered reorderBuffer  -- end of stream

--
listenThreaded :: BSLC.ByteString -> ReaderT Config IO ()
listenThreaded stream = do
    cfg <- ask
    mq <- nextQuote stream
    case mq of
        Just (newQuote, streamRest) -> do
            _ <- liftIO $ tryPutMVar (drawer cfg) (Just newQuote)
            listenThreaded streamRest
        Nothing -> return ()  -- end of stream

-- monitor transaction drawer for new quotes and process them.
-- Because MVars are so slow, this should only be used in conjunction with a skip channel
printer :: ReaderT Config IO ()
printer = do
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
                        logMsg "\nstream appears to have ended."
                        flushBuffer queue
                        liftIO $ putMVar (finishedPrinting cfg) True  -- !!!! prevent deadlock?

--
printerReorder :: Heap.MinHeap Quote -> ReaderT Config IO ()
printerReorder reorderBuffer = do
    cfg <- ask
    mNewQuote <- liftIO . takeMVar $ drawer cfg
    case mNewQuote of
        -- print any quotes that are old enough
        Just newQuote -> do
            newReorderBuffer <- printOlderThan reorderBuffer (acceptTime newQuote)
            printerReorder (Heap.insert newQuote newReorderBuffer)
        Nothing -> do  -- end of stream
            logMsg "\nstream appears to have ended."
            flushReordered reorderBuffer
            liftIO $ putMVar (finishedPrinting cfg) True  -- ensure this doesn't deadlock   !!!!!

--
printerBufferReorder :: Q.MyQueue Quote -> Heap.MinHeap Quote -> ReaderT Config IO ()
printerBufferReorder queue reorderBuffer = do
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
                    Nothing -> do  -- stream ended
                        logMsg "\nstream appears to have ended."
                        flushAll queue reorderBuffer 
                        liftIO $ putMVar  (finishedPrinting cfg) True

-- read all the bytes
myDecodeInt :: BSLC.ByteString -> Int
myDecodeInt bs = myDecodeInt' 0 bs

myDecodeInt' :: Int -> BSLC.ByteString -> Int
myDecodeInt' total bs 
    | BSLC.null bs = total
    | otherwise = myDecodeInt' ((total * 256) + (fromIntegral (BSL.head bs)::Int)) (BSLC.tail bs)

-- get the next quote from the UDP stream
-- Returns Nothing when stream ends
nextQuote :: BSLC.ByteString -> ReaderT Config IO (Maybe (Quote, BSLC.ByteString))
nextQuote stream = if not (BSLC.null stream)
    then do
        cfg <- ask
        -- extract pcap packet header
        let [bsPacketTimeSec, bsPacketTimeUSec, _, bsPacketLen, rest] = splitAts [4,4,4,4] stream
        let [packetTimeSec, packetTimeUSec, packetLen] = map (myDecodeInt . BSLC.reverse) [bsPacketTimeSec, bsPacketTimeUSec, bsPacketLen]
        -- extract ethernet header, IPv4 header, UDP header
        let [_, bsDestPort, rest2] = splitAts[36,2] rest
        if myDecodeInt bsDestPort `elem` [15515, 15516]
        then do
            let [_, keyVal, packetQuote, rest3] = splitAts [4,5,210] rest2
            if BSLC.unpack keyVal == key cfg
            then return $ Just (packetToQuote (show (packetTimeSec * 1000000 + packetTimeUSec)) packetQuote, rest3)
            else nextQuote $ BSLC.drop ((fromIntegral packetLen::Int64)) rest
        else nextQuote $ BSLC.drop ((fromIntegral packetLen::Int64)) rest
    else return Nothing

-- transfers quotes from queue to reorder buffer, while printing from reorder buffer.
-- Then printing quotes remaining in reorder buffer.
flushAll :: Q.MyQueue Quote -> Heap.MinHeap Quote -> ReaderT Config IO ()
flushAll q rb = do
    cfg <- ask
    let (poppedQueue, mQuote) = Q.pop q
    case mQuote of
        Just quote -> do  -- insert popped quote into reorder buffer
            newRB <- printOlderThan rb (acceptTime quote)
            flushAll poppedQueue newRB
        Nothing -> flushReordered rb

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

{- Accept packet time and the 210 bytes after B6034 and turn them into a quote -}
packetToQuote :: String -> BSLC.ByteString -> Quote
{-# INLINE packetToQuote #-}
packetToQuote packetTime bs = Quote
    packetTime
    (items!!0)                      -- issue code
    (items!!3  ++ "@" ++ items!!2)  -- bid 1: qty@price
    (items!!5  ++ "@" ++ items!!4)  -- bid 2
    (items!!7  ++ "@" ++ items!!6)  -- bid 3
    (items!!9  ++ "@" ++ items!!8)  -- bid 4
    (items!!11 ++ "@" ++ items!!10) -- bid 5
    (items!!14 ++ "@" ++ items!!13) -- ask 1
    (items!!16 ++ "@" ++ items!!15) -- ask 2
    (items!!18 ++ "@" ++ items!!17) -- ask 3
    (items!!20 ++ "@" ++ items!!19) -- ask 4
    (items!!22 ++ "@" ++ items!!21) -- ask 5
    (read (items!!24)::Int)         -- accept time
    where
        items = map BSLC.unpack $ init $ splitAts [12,12, 5,7,5,7,5,7,5,7,5,7, 7, 5,7,5,7,5,7,5,7,5,7, 50, 8, 1] bs

-- plural version of splitAt
-- If xs runs out early, copies of remaining xs will be returned.  !!!!!!???
splitAts :: [Int] -> BSLC.ByteString  -> [BSLC.ByteString]
splitAts [] xs = [xs]
splitAts (n:ns) xs = taken:(splitAts ns dropped)
    where (taken, dropped) = BSLC.splitAt (fromIntegral n::Int64) xs  -- xs itself if n > length xs.



