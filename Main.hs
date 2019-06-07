{-
TODO
    haddock comments
    readerT for args
    rest of options
    make sure buffer size positive
    rest used twice
-}

module Main where

import           Data.Foldable(traverse_)
import           Control.Concurrent
import           Control.Concurrent.MVar
import qualified Data.ByteString.Lazy.Char8         as BSLC
import qualified Data.Heap                          as Heap
import           Data.Int (Int64)
--import           Data.Map.Strict (Map, fromList, lookup)
import           Network.Transport.Internal (decodeNum16, decodeNum32)
import           System.Console.GetOpt
import           System.Console.ANSI
import           System.Environment
import           Control.Monad.Reader

-- static global config
data Config = Config {
    filename :: String
    ,sortDelay :: Int
    ,key :: String
    ,ports :: [Int]
    ,reorder :: Bool
    ,ticker :: Bool
    ,buffer :: Bool
    ,bufsize :: Int
    ,maybeSkipchannel :: Maybe (MVar Quote)
}

{- main -}
main :: IO ()
main = do
    -- debugging
    (optArgs, nonOpts, unrecOpts, errs) <- parseArgs
    putStrLn $ "option args: " ++ show optArgs
    putStrLn $ "non options: " ++ show nonOpts
    putStrLn $ "unrecognized options" ++ show unrecOpts
    putStrLn $ "errs" ++ show errs

    -- create skip channel
    skipchan <- newEmptyMVar 

    -- show help if needed
    if (Help `elem` optArgs) then help
    else do

        -- initialize config
        let config = Config {
            filename="mdf-kospi200.20110216-0.pcap" -- PCAP file
            ,sortDelay=300                          -- quote/packet accept time threshold, in hundredths of a second
            ,key="B6034"                            -- target  Data Type + Information Type + Market Type
            ,ports=[15515, 15516]
            ,reorder=(Reorder `elem` optArgs)
            ,ticker=(Ticker `elem` optArgs)
            ,buffer=(hasBuffer optArgs)
            ,bufsize=if length nonOpts > 0 then read (nonOpts!!0)::Int else 0
            ,maybeSkipchannel=if Skip `elem` optArgs then Just skipchan else Nothing
        }
        
        -- fork a thread
        tid <-  myThreadId  -- dirty trick to create tid in scope!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        case maybeSkipchannel config of
            Just skchan -> do
                tid <- forkIO $ skipScanner skchan (ticker config)  -- fork thread

                -- run main if no errors
                case errs of
                    [] -> do
                        stream <- ((BSLC.drop 24) <$> (BSLC.readFile $ filename config))
                        runReaderT (listen stream) config
                        if ticker config then putStr "\n" else return () -- ticker format leaves the cursor at end of line
                    es -> do
                        putStrLn "Option error(s):"
                        traverse_ (\e -> putStrLn $ "    " ++ e) es

                -- kill skip channel thread
                putStrLn "killing skip channel"
                killThread tid

            Nothing -> do

                -- run main if no errors
                case errs of
                    [] -> do
                        stream <- ((BSLC.drop 24) <$> (BSLC.readFile $ filename config))
                        runReaderT (listen stream) config
                        if ticker config then putStr "\n" else return () -- ticker format leaves the cursor at end of line
                    es -> do
                        putStrLn "Option error(s):"
                        traverse_ (\e -> putStrLn $ "    " ++ e) es

-- workaround !!!!!!!!!!
hasBuffer :: [Flag] -> Bool
hasBuffer []     = False
hasBuffer (x:xs) = case x of Buffer _ -> True
                             _ -> hasBuffer xs

--
parseArgs :: IO ([Flag], [String], [String], [String])
parseArgs = do
    cliArgs <- getArgs
    return $ getOpt' Permute options cliArgs

-- show help
help :: IO ()
help = putStrLn $ usageInfo "\nUsage:\n" options

{- -}
data Flag = Help | Skip | Reorder | Ticker | Buffer Int deriving (Show, Eq)  -- Ord

{--}
options :: [OptDescr Flag]
options = [
     Option ['h'] ["help"]         (NoArg Help)      "Show help."
    ,Option ['r'] ["reorder"]      (NoArg Reorder)   "Reorder quotes by quote accept time."
    ,Option ['t'] ["ticker"]       (NoArg Ticker)    "Quotes display on one line, overwriting the previous quote."
    ,Option ['s'] ["skip-channel"] (NoArg Skip)      "Use a skip channel. Skips quotes in order to eliminate lag when printing."
    ,Option ['b'] ["buffer"]       
        (ReqArg (\s -> (Buffer (read s::Int))) "BUFSIZE" )
        "Not recommended. Buffer n quotes before printing. Useful if stream is intermittent and you want to minimize dropped data."
    ]

-- read pcap file
listen :: BSLC.ByteString -> ReaderT Config IO ()
listen bs = 
    if not (BSLC.null bs) then do
        cfg <- ask
        -- extract PCAP header
        let (pcapHeader, rest)              = BSLC.splitAt 16 bs
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
            then do
                dispatch $ packetToQuote packetTime packetQuote
                listen rest4
            else listen $ BSLC.drop pcapLength rest -- rest used twice !!!!!!!!!1
        else listen $ BSLC.drop pcapLength rest -- skip packet
    else liftIO $ return ()

-- dispatch the quote to a buffer, channel, stdio, etc
-- order is:  skip channel --> buffer -> reorder queue
dispatch :: Quote -> ReaderT Config IO ()
dispatch quote = do
    cfg <- ask
    case maybeSkipchannel cfg of
        Just sc -> do
            success <- liftIO $ tryPutMVar sc quote
            liftIO $ return ()
        otherwise -> liftIO $ printQuote quote (ticker cfg)

--
skipScanner :: MVar Quote -> Bool -> IO ()
skipScanner skchan bTicker = do
    q <- takeMVar skchan
    printQuote q bTicker
    skipScanner skchan bTicker

{- printRecursive
-- dispatch :: Heap.MinHeap Quote -> Maybe Int -> IO (Heap.MinHeap Quote)
-- dispatch quoteBuffer newAcceptTime = case newAcceptTime of
    | not $ BSLC.null bs = do  -- Data is arriving from stream
    | not $ Heap.isEmpty quoteBuffer = printQuoteBuffer quoteBuffer Nothing
         >>= listenReordered bs                                         -- stream ended, but buffer not empty
    | otherwise                      = return ()                        -- base case: stream ended and buffer is empty

    Nothing -> case Heap.view quoteBuffer of
        Nothing -> return Heap.empty                -- base case: no new quote + empty buffer
        Just (oldestQuote, poppedBuffer) -> do
            printQuote oldestQuote
            printQuoteBuffer poppedBuffer Nothing   -- no new quote + non-empty buffer:  flush remaining quotes
    Just time -> case Heap.view quoteBuffer of     
        Just (oldestQuote, poppedBuffer) -> do {    -- new quote + non-empty buffer
            if time - (acceptTime oldestQuote) > sortDelay
            then do
                putStrLn $ show $ oldestQuote
                return poppedBuffer                 
            else return quoteBuffer                 -- nothing old enough to print
        }
        Nothing -> return quoteBuffer               -- new quote + empty buffer
-}


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
printQuote :: Quote -> Bool -> IO ()
printQuote quote bTicker = do
    if bTicker then do
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


