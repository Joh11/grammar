{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Grammar where

import Control.Monad.Reader
import Data.Either
import Control.Monad
import Data.Maybe
import Data.Bifunctor
import Data.List

-- Just to differentiate a list of rule from the Kleene star
type KleeneStar a = [a]

emptyWord :: KleeneStar a
emptyWord = []

class Vars v where
  axiom :: v
  
data Grammar :: * -> * -> * where
  Grammar :: (Eq a, Eq v, Vars v) => {grammarRules :: [(v, KleeneStar (Either v a))]} -> Grammar v a

type Language a = [KleeneStar a]

-- First example from Wikipedia
data S = S
  deriving (Show, Eq)
instance Vars S where
  axiom = S

data AB = A | B
  deriving (Show, Eq)

firstGrammar :: Grammar S AB
firstGrammar = Grammar [ (S, [Right A, Left S, Right B])
                       , (S, emptyWord)]


-- Generate all the words of a language from a grammar (potentially infinite)
generateLang :: Grammar v a -> Language a
generateLang  = catMaybes . fmap onlyFullWords . genPartialWords
  where onlyFullWords word | all isRight word = Just $ rights word
                           | otherwise = Nothing

genPartialWords :: Grammar v a -> [PartialWord v a]
genPartialWords (Grammar rules) = runReader (recGenLang (return [[Left axiom]])) rules


recGenLang :: (Eq v, Eq a) => ReaderList [Rule v a] (PartialWord v a) -> ReaderList [Rule v a] (PartialWord v a)
recGenLang m = do
  ws <- fmap (fmap genOne) m
  rules <- ask
  let ws' = nub $ join $ fmap (flip runReader rules) ws
    in if length ws' == length rules
       then m
       else recGenLang $ return ws'

type ReaderList r a = Reader r [a]
type PartialWord v a = KleeneStar (Either v a)
type Rule v a = (v, PartialWord v a)

-- if the partial word has no variables inside then return itself
-- Else return all the possible combinaisons of rules applied to all the variables
genOne :: (Eq v) => PartialWord v a -> ReaderList [Rule v a] (PartialWord v a)
genOne word | length (rights word) == 0 = return $ return word
            | otherwise = do
                rules <- ask
--                 return []
                return $ kindaJoinLeft $ fmap (bimap catMaybes id) $ useAllRules $ fillWithRules word rules

useRule :: (Eq v) => v -> Rule v a -> Maybe (KleeneStar (Either v a))
useRule x (v, m) | x == v = Just m
                 | otherwise = Nothing

fillWithRules :: PartialWord v a -> [Rule v a] -> KleeneStar (Either [(v, Rule v a)] a)
fillWithRules [] _ = []
fillWithRules (Left v : xs) rules = Left (fmap (v,) rules) : fillWithRules xs rules
fillWithRules (Right a : xs) rules = Right a : fillWithRules xs rules

useAllRules :: (Eq v) => KleeneStar (Either [(v, Rule v a)] a) -> KleeneStar (Either [(Maybe (PartialWord v a))] a)
useAllRules = fmap f
  where f (Right a) = Right a
        f (Left xs) = Left $ fmap (uncurry useRule) xs

kindaJoinLeft :: KleeneStar (Either [PartialWord v a] a) -> [PartialWord v a]
kindaJoinLeft [] = []
kindaJoinLeft (Right y : []) = [[Right y]]
kindaJoinLeft (Right y : ys) = fmap ([Right y] ++) $ kindaJoinLeft ys
kindaJoinLeft (Left xs : ys) = [ x ++ y | x <- xs, y <- kindaJoinLeft ys]


retTest :: [a] -> ReaderT r [] a
retTest xs = ReaderT (return xs)
