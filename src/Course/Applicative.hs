{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Applicative(
  Applicative(..)
, sequence
, replicateA
, filtering
, return
, fail
) where

import Course.Core
import Course.Apply
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P

-- | All instances of the `Applicative` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of left identity
--   `∀x. pure id <*> x ≅ x`
--
-- * The law of right identity
--   `∀x. x <*> pure id ≅ x`
class Apply f => Applicative f where
  pure ::
    a -> f a

-- | Witness that all things with (<*>) and pure also have (<$>).
--
-- >>> (+1) <$> (Id 2)
-- Id 3
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
(<$>) ::
  Applicative f =>
  (a -> b)
  -> f a
  -> f b
(<$>) =
  (<*>) . pure

-- | Insert into Id.
--
-- prop> pure x == Id x
instance Applicative Id where
  pure ::
    a
    -> Id a
  pure = Id

-- | Insert into a List.
--
-- prop> pure x == x :. Nil
instance Applicative List where
  pure ::
    a
    -> List a
  pure x = x :. Nil 

-- | Insert into an Optional.
--
-- prop> pure x == Full x
instance Applicative Optional where
  pure ::
    a
    -> Optional a
  pure = Full

-- | Insert into a constant function.
--
-- prop> pure x y == x
instance Applicative ((->) t) where
  pure ::
    a
    -> ((->) t a)
  pure x = \_ -> x 

-- | Sequences a list of structures to a structure of list.
--
-- >>> sequence (Id 7 :. Id 8 :. Id 9 :. Nil)
-- Id [7,8,9]
--
-- >>> sequence ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [[1,1],[1,2],[2,1],[2,2],[3,1],[3,2]]
--
-- >>> sequence (Full 7 :. Empty :. Nil)
-- Empty
--
-- >>> sequence (Full 7 :. Full 8 :. Nil)
-- Full [7,8]
--
-- >>> sequence ((*10) :. (+2) :. Nil) 6
-- [60,8]
sequence ::
  Applicative f =>
  List (f a)
  -> f (List a)
sequence Nil = pure Nil
sequence (x:.xs) = lift2 (:.) x (sequence xs)
-- this one took a few hours... I still don't fully grasp the solution but I could solve it
-- almost solely by going after the type.
-- Of course I knew I had to involve (<$>) and/or (<*>) somehow...
-- After a lot of staring at the types for <$> and <*> and
-- looking at the first example I knew that some intermediary stage would be
-- (Id 7) (Id [8]) => Id [7, 8]
-- If we forget about Id for a moment we would have:
-- f :: a -> (List a) -> (List a)
-- f = (\x y -> x:.y)
-- Note that f can be rewritten as
-- (:.) :: a -> List a -> List a
-- Bringing Id back into the equation that maps perfectly to the type of lift2
-- lift2 :: (a -> b      -> c)      -> f  a -> f  b        -> f  c
-- lift2 :: (a -> List a -> List a) -> Id a -> Id (List a) -> Id (List a)
-- 
                   
-- | Replicate an effect a given number of times.
--
-- >>> replicateA 4 (Id "hi")
-- Id ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 (Full "hi")
-- Full ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 Empty
-- Empty
--
-- >>> replicateA 4 (*2) 5
-- [10,10,10,10]
--
-- >>> replicateA 3 ['a', 'b', 'c']
-- ["aaa","aab","aac","aba","abb","abc","aca","acb","acc","baa","bab","bac","bba","bbb","bbc","bca","bcb","bcc","caa","cab","cac","cba","cbb","cbc","cca","ccb","ccc"]
replicateA ::
  Applicative f =>
  Int
  -> f a
  -> f (List a)
-- replicateA n x = sequence (take n (repeat x)) or
-- replicateA n   = sequence . (take n) . repeat or 
replicateA = (\n -> sequence . (take n) . repeat)
-- This one is easy right after solving sequence!

-- | Filter a list with a predicate that produces an effect.
--
-- >>> filtering (Id . even) (4 :. 5 :. 6 :. Nil)
-- Id [4,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. Nil)
-- Full [4,5,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. Nil)
-- Full [4,5,6,7]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 13 :. 14 :. Nil)
-- Empty
--
-- >>> filtering (>) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. 10 :. 11 :. 12 :. Nil) 8
-- [9,10,11,12]
--
-- >>> filtering (const $ True :. True :.  Nil) (1 :. 2 :. 3 :. Nil)
-- [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]]
--
filtering ::
  Applicative f =>
  (a -> f Bool)
  -> List a
  -> f (List a)
filtering _ Nil = pure $ Nil
filtering f (x:.xs) =
--  (<$>) (\y -> if y then Nil else Nil) (f x)
--  lift2 (++) ((<$>) (\y -> if y then Nil else Nil) (f x))
--  (<$>) (++) (((<$>) (\y -> if y then x:.Nil else Nil) (f x)) filtering f xs)
  (++) <$> ((\y -> if y then (x:.Nil) else Nil) <$> (f x)) <*> (filtering f xs)

-- Took me a while again... I started from the definition of sequence, adopted a bit
-- lift2 (:.) x (filtering f xs)
-- of course this doesn't do the filtering magic yet but
-- f x :: f Bool
-- so how we deal with this? We want to do something like:
-- case f x of
--   True  -> ...
--   False -> ...
-- I was on the wrong track. I was looking for application to the List, however
-- I should have focused on application to the result of (a -> f Bool)
-- Once I figured that out, I was able to come up with:
-- (\y -> if y then a else b) <$> (f x)
-- That was an eye-opener. I assumed the remainer would be trivial but it took me
-- another while as I was first trying to put the recursion to filtering _inside_
-- the if/then/else. I presume there must be a way to somehow make that work
-- but I decided to go down another path:
-- (++) <$> l1 <*> l2 :: f (List a)
-- now it was easy
-- l1 -> either the empty list (if result was false) or the singleton list (if the result was true)
-- l2 -> the recursion into filtering

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Applicative IO where
  pure =
    P.return

instance Applicative [] where
  pure =
    P.return

instance Applicative P.Maybe where
  pure =
    P.return

return ::
  Applicative f =>
  a
  -> f a
return =
  pure

fail ::
  Applicative f =>
  Chars
  -> f a
fail =
  error . hlist
