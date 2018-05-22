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

data (Vars v) => G v a = G [(v, KleeneStar (Either v a))]

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
generateLang :: (Vars v, Eq a) => Grammar v a -> Language a
generateLang grammar = nub $ catMaybes $ fmap onlyFullWords $ genPartialWords grammar [firstPartialWord grammar]
  where onlyFullWords word | all isRight word = Just $ rights word
                           | otherwise = Nothing

genPartialWords :: Grammar v a -> [PartialWord v a] -> [PartialWord v a]
genPartialWords g@(Grammar rules) ws = ws' ++ genPartialWords g ws'
  where ws' = ws ++ runReader (genOneStepLang (return ws)) rules

firstPartialWord :: (Vars v) => Grammar v a -> PartialWord v a
firstPartialWord _ = [Left axiom]

genOneStepLang :: (Eq v, Eq a) => ReaderList [Rule v a] (PartialWord v a) -> ReaderList [Rule v a] (PartialWord v a)
genOneStepLang m = do
  ws <- fmap (fmap genOne) m
  rules <- ask
  prevWords <- m
  return $ filter (not . flip elem prevWords) $ nub $ join $ fmap (flip runReader rules) ws

type ReaderList r a = Reader r [a]
type PartialWord v a = KleeneStar (Either v a)
type Rule v a = (v, PartialWord v a)

-- if the partial word has no variables inside then return itself
-- Else return all the possible combinaisons of rules applied to all the variables
genOne :: (Eq v) => PartialWord v a -> ReaderList [Rule v a] (PartialWord v a)
genOne word | length (lefts word) == 0 = return $ return word
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
kindaJoinLeft (Left xs : []) = xs
kindaJoinLeft (Left xs : ys) = [ x ++ y | x <- xs, y <- kindaJoinLeft ys]
