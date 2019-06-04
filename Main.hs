{-
TODO
    haddock comments
-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.MVar
import qualified Data.ByteString.Lazy.Char8         as BSLC
import qualified Data.Heap                          as Heap
import           Data.Int (Int64)
import           Network.Transport.Internal (decodeNum16, decodeNum32)
import           System.Console.GetOpt
import           System.Console.ANSI
import           System.Environment

{- globals -}
filename    = "mdf-kospi200.20110216-0.pcap" -- PCAP file
sortDelay   = 300::Int                       -- quote/packet accept time threshold, in hundredths of a second
key         = "B6034"                        -- target  Data Type + Information Type + Market Type
ports       = [15515, 15516]

{- main -}
main :: IO ()
main = do
    (optArgs, nonOpts, unrecOpts, errs) <- parseArgs
    putStrLn $ "option args: " ++ show optArgs
    putStrLn $ "non options: " ++ show nonOpts
    putStrLn $ "unrecognized options" ++ show unrecOpts
    putStrLn $ "errs" ++ show errs
    --if Ticker `elem` optArgs then clearScreen else return ()
    if Help `elem` optArgs then help
    else case errs of -- Drop 24-byte global pcap header. 
            [] -> listen
                    (Reorder `elem` optArgs)
                    (Ticker `elem` optArgs)
                    (Skip `elem` optArgs)
                    (Buffer `elem` optArgs)
                    (Just $ (read (nonOpts!!0)::Int))  -- if buffer true
                             =<< ((BSLC.drop 24) <$> (BSLC.readFile filename))
            _ -> help
    if Ticker `elem` optArgs then putStr "\n" else return () -- ticker format leaves the cursor at end of line

--
parseArgs :: IO ([Flag], [String], [String], [String])
parseArgs = do
    cliArgs <- getArgs
    return $ getOpt' Permute options cliArgs

-- show help
help :: IO ()
help = putStrLn $ usageInfo "Usage:\n" options

{- -}
data Flag = Buffer | Help | Skip | Reorder | Ticker | BufferSize String deriving (Eq,Ord,Show)

{--}
options :: [OptDescr Flag]
options = [
    Option ['h'] ["help"]         (NoArg Help)      "Show help."
    ,Option ['r'] ["reorder"]      (NoArg Reorder)   "Reorder quotes by quote accept time."
    ,Option ['t'] ["ticker"]       (NoArg Ticker)    "Quotes display on one line instead of printing multiple lines."
    ,Option ['s'] ["skip-channel"] (NoArg Skip)      "Use a skip channel. Skips quotes in order to eliminate lag when printing."
    ,Option ['b'] ["buffer"]       (NoArg Buffer)    "Use a buffer to collect quotes before printing them. Useful if stream is intermittent and you want to minimize dropped data."
    ,Option ['z'] ["buffer-size"]  (ReqArg BufferSize "BUFSIZE")  "Number of quotes to store in buffer."
    ]

-- read pcap file
listen :: Bool -> Bool -> Bool -> Bool -> Maybe Int -> BSLC.ByteString -> IO ()
listen r t s b z bs =
    if not (BSLC.null bs) then do
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
            if BSLC.unpack keyVal == key
            then do
                dispatch r t s b z $ packetToQuote packetTime packetQuote
                listen r t s b z rest4
            else listen r t s b z $ BSLC.drop pcapLength rest -- rest used twice !!!!!!!!!1
        else listen r t s b z $ BSLC.drop pcapLength rest -- skip packet
    else return ()

-- dispatch the quote to a buffer, channel, stdio, etc
dispatch :: Bool -> Bool -> Bool -> Bool -> Maybe Int -> Quote -> IO ()
-- dispatch :: Heap.MinHeap Quote -> Maybe Int -> IO (Heap.MinHeap Quote)
-- dispatch quoteBuffer newAcceptTime = case newAcceptTime of
dispatch r t s b z quote = do
    if t then do
        --clearFromCursorToLineBeginning
        --clearLine
        --cursorUpLine 1    -- puts cursor at top of screen
        --setCursorPosition 10 0  -- works as expected
        setCursorColumn 0
        putStr $ show quote
    else putStrLn $ show quote
  

{- printRecursive
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
printQuote :: Quote -> IO ()
printQuote quote = putStrLn $ show quote

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


