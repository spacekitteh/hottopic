{-#LANGUAGE GeneralizedNewtypeDeriving, GADTs, DataKinds, StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleContexts, TypeSynonymInstances, OverloadedStrings, FlexibleInstances #-}
module TypeTheory where

import Bound.Scope
import Numeric.Natural 
import Bound.Name
import Data.Text.Lazy hiding (map)
import Text.PrettyPrint.Free
import System.Console.Terminfo.PrettyPrint
import Control.Applicative
import Control.Monad
import Data.Coerce
import Bound.Class
import Prelude.Extras 
import Data.String
data Program a= Program [TDecl a] [VDecl a]

data TDecl a= TDecl (TCons a) [DCons a]
type TCons a = TVar a
type DCons a = TVar a
data VDecl a = VDecl (TVar a) (Expr a)

data Binds = Binding | NonBinding

newtype Universe = Universe Natural deriving (Eq, Ord, Show, Read)


--var n ty = VarExpr (TVar(Var (Name (fromString n) ())) ty) 

lam :: Eq a => a -> Expr a -> Expr a
lam v b = LamExpr (abstract1 v b)



--infixr 9 =:=
--(=:=) :: (Eq a) => Expr a -> Expr a -> Expr a
--a =:=  b = if (a == b) then PrimitiveExpr (Refl a b) else HoleExpr

data Expr a =
    VarExpr a 
    | LamExpr (Scope () Expr a)--a (Expr a)
    | PiExpr  (Scope () Expr a) --a (Expr a)
    | PairExpr  (Expr a) (Expr a) -- (Expr a) a BUGBUGBUG does this need to be a double layer scope to take into account the proper binding?
    | SigmaExpr (Scope () Expr a) --a (Expr a)
    | AppExpr  (Expr a) (Expr a)
    | CaseExpr  (Expr a) [Alt a]
    | UniverseExpr Universe
    | LetExpr [Scope Int Expr a] (Scope Int Expr a) --a (Expr a) (Expr a)
    | PrimitiveExpr (Lit a)
    | HoleExpr deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance Applicative Expr where
    pure = VarExpr
    (<*>) = ap

instance Monad Expr where
    return = VarExpr
    VarExpr a >>= f = f a
    LamExpr e >>= f = LamExpr (e >>>= f)
    PiExpr e >>= f = PiExpr (e >>>= f)
    PairExpr a b >>= f = PairExpr (a >>= f) (b >>= f)
    SigmaExpr e >>= f = SigmaExpr (e >>>= f)
    AppExpr x y >>= f = AppExpr (x >>= f) (y >>= f)
    UniverseExpr a >>= f = UniverseExpr a
    LetExpr bs e >>= f = LetExpr (map (>>>= f) bs) (e >>>= f)
    PrimitiveExpr (IdentityType e1 e2 e3) >>= f = PrimitiveExpr (IdentityType (e1 >>= f) (e2 >>= f) (e3 >>= f))
    PrimitiveExpr (Refl e1 e2) >>= f = PrimitiveExpr (Refl (e1 >>= f) (e2 >>= f))
    PrimitiveExpr CharacterType >>= _ = PrimitiveExpr CharacterType
    PrimitiveExpr (Character c) >>= _ = PrimitiveExpr (Character c)
    PrimitiveExpr FloatingPointType >>= _ = PrimitiveExpr FloatingPointType
    PrimitiveExpr (FloatingPoint d) >>= _ = PrimitiveExpr (FloatingPoint d)
    HoleExpr >>= f = HoleExpr

instance Eq1 Expr      where (==#)      = (==)
instance Ord1 Expr     where compare1   = compare
instance Show1 Expr    where showsPrec1 = showsPrec
instance Read1 Expr    where readsPrec1 = readsPrec

data Alt a = Alt (TCons a) [TCA a] [DCA a] (Expr a) deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
type TCA a = TVar a
type DCA a = TVar a

data Lit a = IdentityType (Expr a) (Expr a) (Expr a) -- a A b
         | Refl (Expr a) (Expr a)
         | CharacterType
         | Character Char
         | FloatingPointType
         | FloatingPoint Double deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)
{-       | ZeroType
         | OneType
         | OneObject
         | NatType
         | ZeroNat
         | SuccNat (Expr a)-}


data TVar a = TVar (Var a) (Expr a) deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Read)
newtype Var a = Var (Name Text a) deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Read)


data DeltaRule a = DeltaRule (TVar a) (Expr a) deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Read)
type DeltaRules a= [DeltaRule a]

data Redex a = NoRedex
           | BetaRedex
           | IotaRedex
           | DeltaRedex (DeltaRule a)
           | ZetaRedex

prog2DeltaRules :: Program a -> DeltaRules a
prog2DeltaRules (Program _ vdecls) = map vDecl2DeltaRule vdecls

vDecl2DeltaRule :: VDecl a -> DeltaRule a
vDecl2DeltaRule (VDecl tv ex) = DeltaRule tv ex

nf :: Expr a -> Expr a
nf HoleExpr = HoleExpr




{-data Sequent f i e a = Sequent (Context f i e a) (Term f i e a) 
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

-}



