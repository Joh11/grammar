module Grammar.Example( S(..)
                      , AB(..)
                      , firstGrammar) where

import Grammar.Grammar

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
