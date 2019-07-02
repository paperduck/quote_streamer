module View
( logMsg
, printQuote
, showErrors
) where

import           Control.Monad.Reader       (ReaderT, runReaderT, ask, asks, liftIO)
import           Data.List                  (nub)
import           Data.Foldable              (traverse_)
import           System.Console.ANSI        (setCursorColumn) 

import           Model                      (Config(..), Flag(..), Quote(..))

-- pretty print errors 
showErrors :: [String] -> IO ()
showErrors es = putStrLn "\nPlease fix the following error(s):" >>
    traverse_ (\e -> putStrLn $ "    -> " ++ e) (nub es)

--
logMsg :: String -> ReaderT Config IO ()
{-# INLINE logMsg #-}
logMsg msg = asks verbose >>= (\v -> if v then liftIO . putStrLn $ msg else return ())

{- Prints a Quote. -}                                                                                                                                                                                 
printQuote :: Quote -> ReaderT Config IO () 
{-# INLINE printQuote #-} 
printQuote quote = do
    cfg <- ask
    if ticker cfg then liftIO $ do
        setCursorColumn 0
        putStr $ show quote
    else
        liftIO . putStrLn $ show quote
