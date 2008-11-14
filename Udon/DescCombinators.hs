module Udon.DescCombinators where

import Udon.DataDesc
import Data.Maybe (isJust, listToMaybe)
import Control.Monad (guard)

unit :: DataDesc ()
unit = pure ()

pair :: DataDesc a -> DataDesc b -> DataDesc (a,b)
pair p q = sequ fst p $ \x ->
           sequ snd q $ \y ->
           pure (x,y)

wrap :: (a -> b, b -> a) -> DataDesc a -> DataDesc b
wrap (i,j) p = sequ j p (pure . i)

fixedList :: DataDesc a -> Int -> DataDesc [a]
fixedList p 0 = pure []
fixedList p n = wrap consIso (pair p (fixedList p (n-1)))
    where
    consIso = (\(a,b) -> a:b, \(a:b) -> (a,b))

list :: DataDesc a -> DataDesc [a]
list = sequ length binary . fixedList

data Pattern a where
    Pattern :: (a -> Maybe b) -> DataDesc b -> (b -> a) -> Pattern a

-- The first argument is a notation hack.  See descMaybe and descEither for
-- usage.
match :: ([a] -> [b]) -> DataDesc b -> (b -> a) -> Pattern a
match f = Pattern (listToMaybe . f . return)

alt :: forall a. [Pattern a] -> DataDesc a
alt pats = 
    sequ tag binary $ \idx -> 
    case pats !! idx of
        Pattern match descb t -> wrap (t, myFromJust . match) descb
    where
    tag :: a -> Int
    tag x = myHead $ do
                (n, Pattern pat _ _) <- zip [0..] pats
                guard (isJust (pat x))
                return n
    
    myHead [] = error "Non-exhaustive patterns in alternation"
    myHead (x:xs) = x

    myFromJust Nothing = error "Ack! Data inconsistency!"
    myFromJust (Just x) = x

descMaybe :: DataDesc a -> DataDesc (Maybe a)
descMaybe p = alt [
    match (\x -> [()| Nothing <- x]) unit (const Nothing),
    match (\x -> [y | Just y  <- x])  p    Just ]

descEither :: DataDesc a -> DataDesc b -> DataDesc (Either a b)
descEither p q = alt [
    match (\x -> [y | Left y  <- x]) p Left,
    match (\x -> [y | Right y <- x]) q Right ]
