-- Queue wrapper around Data.Sequence

module MyQueue
(MyQueue
,empty
,push
,pop
,peek
,size
,MyQueue.null
) where

import qualified Data.Sequence as S

newtype MyQueue a = MyQueue (S.Seq a)

-- return an empty queue
empty :: MyQueue a
{-# INLINE empty #-}
empty = MyQueue S.empty

-- push onto tail
push :: a -> MyQueue a -> MyQueue a
{-# INLINE push #-}
push v (MyQueue s) = MyQueue ( v S.<| s )

-- pop from head
pop :: MyQueue a -> (MyQueue a, Maybe a)
{-# INLINE pop #-}
pop q = case q of
    MyQueue S.Empty -> (q, Nothing)
    MyQueue (xs S.:|> x) -> (MyQueue xs, Just x)

-- look at the head
peek :: MyQueue a -> Maybe a
{-# INLINE peek #-}
peek q = case q of
    MyQueue S.Empty -> Nothing
    MyQueue (xs S.:|> x) -> Just x

-- get number of items
size :: MyQueue a -> Int
{-# INLINE size #-}
size (MyQueue s) = S.length s

-- is the queue empty?
null :: MyQueue a -> Bool
{-# INLINE null #-}
null (MyQueue s) = S.null s



