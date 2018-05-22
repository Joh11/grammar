{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Grammar.Grammar( KleeneStar
                      , emptyWord
                      , Vars
                      , axiom
                      , Grammar(..)
                      , Language
                      , PartialWord
                      , Rule) where

-- Just to differentiate a list of rule from the Kleene star
type KleeneStar a = [a]

emptyWord :: KleeneStar a
emptyWord = []

class Vars v where
  axiom :: v
  
data Grammar :: * -> * -> * where
  Grammar :: (Eq a, Eq v, Vars v) => {grammarRules :: [(v, KleeneStar (Either v a))]} -> Grammar v a

type Language a = [KleeneStar a]

type PartialWord v a = KleeneStar (Either v a)
type Rule v a = (v, PartialWord v a)
