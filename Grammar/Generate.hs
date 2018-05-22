{-# LANGUAGE TupleSections #-}
module Grammar.Generate(generateLang) where

import Data.List
import Data.Maybe
import Data.Either
import Control.Monad
import Data.Bifunctor

import Grammar.Grammar

-- Generate all the words of a language from a grammar (potentially infinite)
generateLang :: (Vars v, Eq a) => Grammar v a -> Language a
generateLang grammar = nub $ catMaybes $ fmap onlyFullWords $ genPartialWords grammar [firstPartialWord grammar]
  where onlyFullWords word | all isRight word = Just $ rights word
                           | otherwise = Nothing

genPartialWords :: Grammar v a -> [PartialWord v a] -> [PartialWord v a]
genPartialWords g@(Grammar rules) ws = ws' ++ genPartialWords g ws'
  where ws' = ws ++ genOneStepLang rules ws

firstPartialWord :: (Vars v) => Grammar v a -> PartialWord v a
firstPartialWord _ = [Left axiom]

genOneStepLang :: (Eq v, Eq a) => [Rule v a] -> [PartialWord v a] -> [PartialWord v a]
genOneStepLang rules prevWords = let ws = fmap (genOne rules) prevWords
                                 in filter (not . flip elem prevWords) $ nub $ join $  ws


-- if the partial word has no variables inside then return itself
-- Else return all the possible combinaisons of rules applied to all the variables
genOne :: (Eq v) => [Rule v a] -> PartialWord v a -> [PartialWord v a]
genOne rules word | length (lefts word) == 0 = []
                  | otherwise = kindaJoinLeft $ fmap (first catMaybes) $ useAllRules $ fillWithRules word rules

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
