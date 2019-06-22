module MyQueue
( MyQueue
, empty
, push
, pop
, peek
, size
--, pattern Empty
) where

import qualified Data.Sequence as S

newtype MyQueue a = MyQueue (S.Seq a)

empty :: MyQueue a
empty = MyQueue S.empty

-- push onto tail
push :: a -> MyQueue a -> MyQueue a
push v q = case q of
    MyQueue s -> MyQueue ( v S.<| s )

-- pop from head
pop :: MyQueue a -> (MyQueue a, Maybe a)
pop q = case q of
    MyQueue S.Empty -> (q, Nothing)
    MyQueue (xs S.:|> x) -> (MyQueue xs, Just x)

-- look at the head
peek :: MyQueue a -> Maybe a
peek q = case q of
    MyQueue S.Empty -> Nothing
    MyQueue (xs S.:|> x) -> Just x

-- get number of items
size :: MyQueue a -> Int
size q = case q of 
    MyQueue s -> S.length s






