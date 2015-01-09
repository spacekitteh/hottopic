{-#LANGUAGE GeneralizedNewtypeDeriving, GADTs, DataKinds, StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Theories.HomotopyTypeTheory where

import Numeric.Natural
import Text.PrettyPrint.Free
import System.Console.Terminfo.PrettyPrint

import qualified TypeTheory as TT

type Expr  identifier = TT.Term Formation Introduction Elimination identifier

data DefinedConstant identifier = Fgsfds deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
data Formation identifier =  PiType  (Expr identifier) (Expr identifier) -- x B -> \~\_(x:A) B
                           | SigmaType (Expr identifier) (Expr identifier) -- a B -> \Sigma_(x:A) B 
                            deriving (Eq, Ord,  Functor, Foldable, Traversable)
data Introduction identifier = Universe Natural 
                               | LambdaAbstraction (Expr identifier) (Expr identifier)  -- x b -> \(x:A) . b : |~|_(x:A) B
                               | DependantPair (Expr identifier) (Expr identifier)-- (a, b:B)
                                 deriving (Eq, Functor, Ord, Foldable, Traversable)
data Elimination identifier = LambdaApplication (Expr identifier) (Expr identifier) -- (f:|~|_(x:A) B) a -> B[a/x]
                                      deriving (Eq, Functor,  Ord, Foldable, Traversable)
data Computation identifier = ComputePiType (Expr identifier) (Expr identifier) -- f a := b[a/x] : B[a/x]
                                      deriving (Eq, Functor, Ord, Foldable, Traversable)
data UniquenessRule identifier = PiUniqueness 
                                     deriving (Eq, Functor,  Ord, Foldable, Traversable)

instance Pretty identifier => Pretty (Formation identifier) where
    pretty (PiType x b) = char '∏' <> (pretty x) <> char '→' <> (pretty b)
    pretty (SigmaType a b) = char '∑' <> (pretty a) <> char '|' <> pretty b
instance Pretty identifier => Pretty (Elimination identifier) where
    pretty (LambdaApplication f x) = parens (pretty f) <> parens (pretty x)
instance Pretty identifier => Pretty (Introduction identifier) where
    pretty (Universe level) = text "𝓤_" <> pretty level

instance Pretty identifier => Show (Formation identifier) where
    show = show . pretty
instance Pretty identifier => Show (Introduction identifier) where
    show = show . pretty
instance Pretty identifier => Show (Elimination identifier) where
    show = show . pretty
