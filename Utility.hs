module Utility
(splitAts
,myDecodeInt16
,myDecodeInt32
) where

import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy       as BSL
import           Data.Int                   (Int16, Int32, Int64)

-- plural version of splitAt
-- If xs runs out early, partial or empty byestrings are returned
splitAts :: [Int] -> BSLC.ByteString  -> [BSLC.ByteString]
splitAts [] xs = [xs]
splitAts (n:ns) xs = taken:(splitAts ns dropped)
    where (taken, dropped) = BSLC.splitAt (fromIntegral n::Int64) xs  -- xs itself if n > length xs.

--
myDecodeInt16 :: BSLC.ByteString -> Int
myDecodeInt16 bs = if BSLC.length bs == 2 
                    then myDecodeInt' 0 bs 
                    else error "myDecodeInt16: Not 2 bytes"
--
myDecodeInt32 :: BSLC.ByteString -> Int
myDecodeInt32 bs = if BSLC.length bs == 4 
                    then myDecodeInt' 0 bs 
                    else error "myDecodeInt32: Not 4 bytes"
-- This goes on forever so it's crucial to check number of bytes before calling
myDecodeInt' :: Int -> BSLC.ByteString -> Int
myDecodeInt' total bs
    | BSLC.null bs = total
    | otherwise = myDecodeInt' ((total * 256) + (fromIntegral (BSL.head bs)::Int)) (BSLC.tail bs)
