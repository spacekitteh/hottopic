{-#LANGUAGE GeneralizedNewtypeDeriving, GADTs, DataKinds, StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
module TypeTheory where

import Bound.Scope
import Numeric.Natural 
import Bound.Name
import Data.Text.Lazy
import Text.PrettyPrint.Free
import System.Console.Terminfo.PrettyPrint


data Sequent formation introduction elimination identifier = Sequent (Context formation introduction elimination identifier) (Context formation introduction elimination identifier) 
data Rule formation introduction elimination identifier = Rule String [Sequent formation introduction elimination identifier] (Sequent formation introduction elimination identifier)

newtype Context formation introduction elimination identifier = Context[(Var identifier, Term formation introduction elimination identifier)] deriving (Show, Eq, Ord, Foldable, Traversable, Functor)


data Judgement formation introduction elimination identifier = ValidContext (Context formation introduction elimination identifier) 
 | TermInhabits (Context formation introduction elimination identifier) (Term formation introduction elimination identifier) (Term formation introduction elimination identifier) 
 | JudgementallyEqual (Context formation introduction elimination identifier) (Term formation introduction elimination identifier) (Term formation introduction elimination identifier) (Term formation introduction elimination identifier) 
 deriving (Eq, Ord, Show, Functor, Foldable, Traversable)



type Var identifier = Name Text identifier

instance Pretty (Var identifier) where
    pretty (Name n _) = pretty n

data Term formation introduction elimination identifier = 
    Variable (Var identifier) (Term formation introduction elimination identifier)   
    | TypeFormation (formation identifier)
    | Constructor (introduction identifier)
    | Application  (elimination identifier) 
    | Hole deriving (Eq, Ord, Functor, Foldable, Traversable)

instance (Pretty (formation identifier), Pretty (introduction identifier), Pretty (elimination identifier), Pretty identifier) => Pretty (Term formation introduction elimination identifier) where
    pretty Hole = char '_'
    pretty (Variable ident ty) = pretty ident <> char ':' <> pretty ty
    

instance (Pretty (formation identifier), Pretty (introduction identifier), Pretty (elimination identifier), Pretty identifier) => Show (Term formation introduction elimination identifier) where
    show = show . pretty

--



