{-#LANGUAGE GeneralizedNewtypeDeriving, GADTs, DataKinds, StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleContexts, TypeSynonymInstances, OverloadedStrings, FlexibleInstances #-}
module TypeTheory where

import Bound.Scope
import Numeric.Natural 
import Bound.Name
import Data.Text.Lazy
import Text.PrettyPrint.Free
import System.Console.Terminfo.PrettyPrint
import Control.Applicative
import Control.Monad
data Sequent f i e a = Sequent (Context f i e a) (Term f i e a) 
data Rule f i e a = Rule String [Sequent f i e a] (Sequent f i e a)

newtype Context f i e a = Context[(Var a, Term f i e a)] deriving (Show, Eq, Ord, Foldable, Traversable, Functor)


data Judgement f i e a = ValidContext (Context f i e a) 
 | TermInhabits (Context f i e a) (Term f i e a) (Term f i e a) 
 | JudgementallyEqual (Context f i e a) (Term f i e a) (Term f i e a) (Term f i e a) 
 deriving (Eq, Ord, Show, Functor, Foldable, Traversable)



newtype Var a = MkVar (Name Text a) deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Pretty (Var a) where
    pretty (MkVar n) = (pretty . name) n

data Term f i e a = 
    Variable a
    | Type (f a)
    | Constructor (i a)
    | Application  (e a) 
    | Hole deriving (Eq, Ord, Functor, Foldable, Traversable)


instance (Functor f, Functor i, Functor e) => Applicative (Term f i e) where
    pure = return
    (<*>) = ap

instance (Functor f, Functor i, Functor e) => Monad (Term f i e) where
    return = Variable
    



instance (Pretty (f a), Pretty (i a), Pretty (e a), Pretty a) => Pretty (Term f i e a) where
    pretty Hole = char '_'
    pretty (Variable ident) = pretty ident -- <> char ':' <> pretty ty
    

instance (Pretty (f a), Pretty (i a), Pretty (e a), Pretty a) => Show (Term f i e a) where
    show = show . pretty

--



