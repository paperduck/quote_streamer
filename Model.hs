module Model
( Config(..)
, Flag (..)
, Quote(..)
) where

import           Control.Concurrent.MVar    (MVar)

-- Environment configuration settings
data Config = Config {
     filename           :: !String
    ,verbose            :: !Bool
    ,sortDelay          :: !Int
    ,quoteType          :: !String
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

-- Command line option flag
data Flag = Verbose | Help | Skip | Reorder | Ticker | Buffer Int 
    deriving (Show, Eq)

-- Quote data type
data Quote = Quote {                      
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

-- `Show` instance for `Quote`
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

-- `Ord` instance for `Quote`
instance Ord Quote where
    compare q1 q2 = compare (acceptTime q1, packetTime q1) (acceptTime q2, packetTime q2)


