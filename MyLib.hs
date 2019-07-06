module MyLib 
(appMain
) where

-- Dependencies
import           Control.Concurrent         (forkIO)
import           Control.Concurrent.MVar    (takeMVar, putMVar, tryTakeMVar
                                            , tryPutMVar, newEmptyMVar)
import           Control.Monad.Reader       (runReaderT, ReaderT, ask, liftIO
                                            , liftM)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Heap                  as Heap
import           Data.Int                   (Int64)
import           System.Console.GetOpt      (getOpt', OptDescr(Option)
                                            , ArgDescr(ReqArg, NoArg)
                                            , usageInfo
                                            , ArgOrder(Permute))
import           System.Environment         (getArgs)
import           System.TimeIt              (timeIt)
-- Other modules
import           Model                      (Config(..), Flag(..), Quote(..))
import qualified MyQueue                    as Q
import           Utility                    (splitAts, myDecodeInt16
                                            , myDecodeInt32)
import           View                       (logMsg, printQuote, showErrors)

-- main
appMain :: IO ()
appMain = timeIt $ do
    -- Gather command line options
    (optArgs, nonOpts, unrecOpts, cmdErrs)  <- liftM (getOpt' Permute options) getArgs

    -- Environment configuration settings
    emptyDrawer                 <- newEmptyMVar 
    finishedPrintingSemaphore   <- newEmptyMVar
    endOfStreamSemaphore        <- newEmptyMVar
    let bufferSize  = getBufferSize optArgs
    let config      = Config {
        filename            ="mdf-kospi200.20110216-0.pcap"
        ,verbose            =(Verbose `elem` optArgs)
        ,sortDelay          =300      -- accept time threshold in 1/100 seconds 
        ,quoteType          ="B6034"  -- target  Data Type + Information Type + Market Type
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
    runReaderT (logMsg $ 
           "\nOption arguments:     " ++ show optArgs
        ++ "\nNon options:          " ++ show nonOpts
        ++ "\nUnrecognized options: " ++ show unrecOpts
        ++ "\nOption errors:        " ++ show cmdErrs    ) config
    
    -- Gather all error messages
    let errorMsgs = cmdErrs
            -- Unrecognized options
            ++ (map ("Unrecognized option: " ++) unrecOpts)
            -- BUFSIZE must be > 0
            ++ if (buffer config) && (bufsize config <= 0)
                then ["Buffer size must be greater than zero."]
                else []
            -- Buffer pointless when not multithreaded
            ++ if not (skip config) && buffer config
                then ["Buffer option cannot be used without skip channel."]
                else []
    --
    case errorMsgs of
        [] -> if (Help `elem` optArgs)  -- Prioritize help
            then putStrLn $ usageInfo "\nUsage:\n" options
            else do
                -- Open pcap file and drop pcap header
                stream <- liftM (BSLC.drop 24) (BSLC.readFile $ filename config)
                if skip config then do
                    -- Launch a printer thread before the stream starts
                    if buffer config then
                        if reorder config then forkIO $ runReaderT (printerBufferReorder Q.empty Heap.empty) config
                        else                   forkIO $ runReaderT (printerBuffer        Q.empty)            config
                    else
                        if reorder config then forkIO $ runReaderT (printerReorder               Heap.empty) config
                        else                   forkIO $ runReaderT (printer                                ) config
                    -- Start stream
                    runReaderT (listenThreaded stream) config
                else
                    if reorder config then runReaderT (listenReorder stream Heap.empty) config
                    else                   runReaderT (listen        stream)            config
                -- Once the stream ends, wait for the printer thread to terminate
                if skip config then do
                    runReaderT (logMsg "\nSignalling end of stream.") config
                    -- Tell printer thread to flush
                    putMVar (drawer config) Nothing
                    runReaderT (logMsg "\nWaiting for printer thread to finish.") config 
                    -- Block until flushed
                    finished <- takeMVar $ finishedPrinting config
                    runReaderT (logMsg "\nPrinter thread appears to have finished.") config
                else return ()
                -- Ticker leaves cursor at end of line
                if ticker config then putStrLn "" else return ()
        es -> showErrors es

-- RETURNS: The buffer size, if it exists, from the option flags
getBufferSize :: [Flag] -> Maybe Int
{-# INLINE getBufferSize #-}
getBufferSize flags = case sizes of
    x:_ -> Just x
    [] -> Nothing
    where sizes = [size | Buffer size <- flags]

-- RETURNS: A description of command line options
options :: [OptDescr Flag]
{-# INLINE options #-}
options = [ 
     Option ['v'] ["verbose"] (NoArg Verbose)   "Verbose output"
    ,Option ['h'] ["help"]    (NoArg Help)      "Show help"
    ,Option ['r'] ["reorder"] (NoArg Reorder)   "Reorder by quote accept time"
    ,Option ['t'] ["ticker"]  (NoArg Ticker)    "Each quote overwrites the previous one"
    ,Option ['s'] ["skip-channel"] (NoArg Skip) "Skip quotes to eliminate printing bottleneck."
    ,Option ['b'] ["buffer"]  (ReqArg (\s -> Buffer (read s::Int)) "BUFSIZE")
        "Buffer up to n quotes to be printed. Not as useful as it seems."
    ]

-- Take a quote from the stream and print it
listen :: BSLC.ByteString -> ReaderT Config IO ()
listen stream = do
    cfg <- ask
    mq <- nextQuote stream
    case mq of
        Just (newQuote, streamRest) -> do
            printQuote newQuote
            listen streamRest
        Nothing -> return ()  -- end of stream

-- Take a quote from the stream and reorder it
listenReorder :: BSLC.ByteString -> Heap.MinHeap Quote -> ReaderT Config IO ()
listenReorder stream reorderBuffer = do
    cfg <- ask
    mq <- nextQuote stream
    case mq of
        Just (newQuote, streamRest) -> do
            poppedReorderBuffer <- printOlderThan reorderBuffer (acceptTime newQuote)
            listenReorder streamRest (Heap.insert newQuote poppedReorderBuffer)
        Nothing -> flushReordered reorderBuffer  -- end of stream

-- Take a quote from the stream and hand it to a printer thread
listenThreaded :: BSLC.ByteString -> ReaderT Config IO ()
listenThreaded stream = do
    cfg <- ask
    mq <-  nextQuote stream
    case mq of
        Just (newQuote, streamRest) -> do
            _ <- liftIO $ tryPutMVar (drawer cfg) (Just newQuote)
            listenThreaded streamRest
        Nothing -> return ()  -- end of stream

-- Take a quote from the drawer and print it
printer :: ReaderT Config IO ()
printer = do
    cfg <- ask
    mNewQuote <- liftIO . takeMVar $ drawer cfg
    case mNewQuote of
        Nothing -> do
            logMsg "\nstream appears to have ended"
            liftIO $ putMVar (finishedPrinting cfg) True
        Just newQuote -> printQuote newQuote >> printer

-- Take a quote from the drawer and buffer it. 
-- If buffer full, pop a quote and print that instead.
printerBuffer :: Q.MyQueue Quote -> ReaderT Config IO ()
printerBuffer queue = do
    cfg <- ask
    mmNewQuote <- liftIO . tryTakeMVar $ drawer cfg
    case mmNewQuote of
        Just mNewQuote -> handleMNewQuote mNewQuote  -- Maybe new quote
        Nothing -> do  -- No new quote; use this opportunity to pop buffer
            let (poppedQueue, mQuote) = Q.pop queue
            case mQuote of
                -- Buffer is empty, so we can block until next quote arrives
                Nothing -> liftIO (takeMVar $ drawer cfg) >>= handleMNewQuote
                -- Pop quote from buffer and print it
                Just quote -> printQuote quote >> printerBuffer poppedQueue
        where handleMNewQuote mNewQuote = do
                cfg <- ask
                case mNewQuote of
                    Just newQuote -> if Q.size queue >= bufsize cfg
                        then do  -- Buffer full; skip new quote
                            let (poppedQueue, mQuote) = Q.pop queue
                            case mQuote of
                                Just quote -> printQuote quote >> printerBuffer poppedQueue
                                Nothing -> error "printerBuffer: full queue has no items"
                        -- Quickly add to buffer without printing anything
                        else printerBuffer (Q.push newQuote queue)  
                    Nothing -> do  -- End of stream has been signalled
                        logMsg "\nstream appears to have ended."
                        flushBuffer queue
                        liftIO $ putMVar (finishedPrinting cfg) True

-- Take a quote from the drawer and reorder it.
printerReorder :: Heap.MinHeap Quote -> ReaderT Config IO ()
printerReorder reorderBuffer = do
    cfg <- ask
    mNewQuote <- liftIO . takeMVar $ drawer cfg
    case mNewQuote of
        Just newQuote -> do  -- Print any quotes that are old enough
            newReorderBuffer <- printOlderThan reorderBuffer (acceptTime newQuote)
            printerReorder (Heap.insert newQuote newReorderBuffer)
        Nothing -> do  -- End of stream
            logMsg "\nstream appears to have ended."
            flushReordered reorderBuffer
            liftIO $ putMVar (finishedPrinting cfg) True

-- Take a quote from the drawer and buffer it. 
-- If buffer full or no new quote, pop a quote and reorder it.
printerBufferReorder :: Q.MyQueue Quote -> Heap.MinHeap Quote -> ReaderT Config IO ()
printerBufferReorder queue reorderBuffer = do
    cfg <- ask
    mmNewQuote <- liftIO . tryTakeMVar $ drawer cfg 
    case mmNewQuote of
        Just mNewQuote -> handleMNewQuote mNewQuote -- Maybe new quote
        Nothing -> do  -- No new quote; use this opportunity to pop buffer
            let (poppedQueue, mQuote) = Q.pop queue
            case mQuote of
                -- Buffer is empty, so we can block until a new quote arrives.
                Nothing -> (liftIO . takeMVar $ drawer cfg) >>= handleMNewQuote
                -- Pop quote from buffer and reorder it
                Just quote -> do  
                    newReorderBuffer <- printOlderThan reorderBuffer (acceptTime quote)
                    printerBufferReorder poppedQueue (Heap.insert quote newReorderBuffer)
        where handleMNewQuote mNewQuote = do
                cfg <- ask
                case mNewQuote of
                    Just newQuote -> if Q.size queue >= bufsize cfg
                        then do   -- Buffer full; skip new quote
                            let (poppedQueue, mQuote) = Q.pop queue
                            case mQuote of
                                Just quote ->  do  -- Insert to reorder buffer
                                    newReorderBuffer <- printOlderThan reorderBuffer (acceptTime quote)
                                    printerBufferReorder poppedQueue newReorderBuffer
                                Nothing -> error "printerBufferReorder: full queue has no items"
                        -- Quickly add to buffer without printing anything
                        else printerBufferReorder (Q.push newQuote queue) reorderBuffer
                    Nothing -> do  -- Stream ended
                        logMsg "\nstream appears to have ended."
                        flushAll queue reorderBuffer 
                        liftIO $ putMVar  (finishedPrinting cfg) True

-- RETURNS: The next quote from the UDP stream, or Nothing when
-- stream ends.
nextQuote :: BSLC.ByteString -> ReaderT Config IO (Maybe (Quote, BSLC.ByteString))
nextQuote stream = if not (BSLC.null stream)
    then do
        cfg <- ask
        -- Extract pcap packet header
        let [bsPacketTimeSec, bsPacketTimeUSec, _, bsPacketLen, rest] = splitAts [4,4,4,4] stream
        let [packetTimeSec, packetTimeUSec, packetLen] 
                = map
                    (myDecodeInt32 . BSLC.reverse)
                    [bsPacketTimeSec, bsPacketTimeUSec, bsPacketLen]
        -- Extract ethernet header, IPv4 header, UDP header
        let [_, bsDestPort, rest2] = splitAts[36,2] rest
        if myDecodeInt16 bsDestPort `elem` (ports cfg)  -- Filter by port
        then do
            let [_, quoteTypeVal, packetQuote, rest3] = splitAts [4,5,210] rest2
            if BSLC.unpack quoteTypeVal == (quoteType cfg)  -- Filter by quote type
            then return $ Just
                ( packetToQuote (packetTimeSec * 1000000 + packetTimeUSec) packetQuote 
                , rest3 )
            else nextQuote $ BSLC.drop ((fromIntegral packetLen::Int64)) rest
        else     nextQuote $ BSLC.drop ((fromIntegral packetLen::Int64)) rest
    else return Nothing

-- Transfers quotes from queue to reorder buffer, while printing from reorder buffer.
-- Then printing quotes remaining in reorder buffer.
flushAll :: Q.MyQueue Quote -> Heap.MinHeap Quote -> ReaderT Config IO ()
flushAll q rb = do
    cfg <- ask
    let (poppedQueue, mQuote) = Q.pop q
    case mQuote of
        -- Insert popped quote into reorder buffer
        Just quote -> printOlderThan rb (acceptTime quote) >>= flushAll poppedQueue
        Nothing -> flushReordered rb

-- Print quotes remaining in buffer
flushBuffer :: Q.MyQueue Quote -> ReaderT Config IO ()
flushBuffer queue = do
    cfg <- ask
    let (poppedBuf, mQuote) = Q.pop queue
    case mQuote of
        Just q -> printQuote q >> flushBuffer poppedBuf
        Nothing -> return ()

-- Print remaining reordered quotes 
flushReordered :: Heap.MinHeap Quote -> ReaderT Config IO ()
flushReordered buf = do
    cfg <- ask
    case Heap.view buf of
        Just (oldestQuote, poppedBuf) -> do
            printQuote oldestQuote
            flushReordered poppedBuf
        Nothing -> return ()

-- RETURNS: The modified heap after printing ripe quotes
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

-- Accept packet time and the 210 bytes after the QUOTE TYPE
-- RETURNS: A new quote made from the bytes
packetToQuote :: Int -> BSLC.ByteString -> Quote
{-# INLINE packetToQuote #-}
packetToQuote packetTime bs = Quote
    (show packetTime)
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
