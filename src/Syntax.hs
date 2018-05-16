{-# LANGUAGE LambdaCase #-}

module Syntax
  ( module Syntax
  , Text
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text.Lazy (Text, pack, unpack)

----------------------------------------
-- | Variables
----------------------------------------

type Var = Text

mkVar :: String -> Var
mkVar = pack

showVar :: Var -> String
showVar = unpack

----------------------------------------
-- | Top level declarations
----------------------------------------

data Decl
  = BindD Bind
  | SigD Var Type
  | TypeD Var Type
  | InfixD Fixity Var Var
  deriving (Show, Eq)


-- | Predicates
isInfixD :: Decl -> Bool
isInfixD InfixD {} = True
isInfixD _         = False

isBindD :: Decl -> Bool
isBindD BindD {} = True
isBindD _        = False

isSigD :: Decl -> Bool
isSigD SigD {} = True
isSigD _       = False

isTypeD :: Decl -> Bool
isTypeD TypeD {} = True
isTypeD _        = False

----------------------------------------
-- | Binds
----------------------------------------

data Bind
  = ValB Var Expr
  | FunB Var Expr
  deriving (Show, Eq)

-- | Split a bind
getBind :: Bind -> (Var, Expr)
getBind (ValB v e) = (v, e)
getBind (FunB v e) = (v, e)

----------------------------------------
-- | Expressions
----------------------------------------

data Expr
  = VarE Var
  | AppE Expr Expr
  | LamE Var Expr
  | LetE Bind Expr
  | LitE Literal
  | InfixE Var Expr Expr
  | IfE Expr Expr Expr
  | CaseE Expr [Alt]
  | FixE Expr
  | TupE [Expr]
  | SumE (Either Expr Expr)
  | AsE Expr Type
  deriving (Show, Eq)

----------------------------------------
-- | Literals
----------------------------------------

data Literal
  = IntL Integer
  | RealL Double
  | StringL Text
  | BoolL Bool
  deriving (Show, Eq)

----------------------------------------
-- | Case alternatives
----------------------------------------

data Alt = Alt Pat Expr
  deriving (Show, Eq)

----------------------------------------
-- | One level patterns
----------------------------------------

data Pat
  = LitP Literal
  | VarP Var
  | TupP [Var]
  | SumP (Either Var Var)
  deriving (Show, Eq)

----------------------------------------
-- | Fixities
----------------------------------------

data Fixity = L | R
  deriving (Show, Eq)

----------------------------------------
-- | Types
----------------------------------------

data Type
  = VarT Var
  | ConT Var
  | Type :->: Type
  | Type :+: Type
  | TupT [Type]
  deriving (Show, Eq, Ord)

infix  :+:
infixr :->:

-- | Primitive types
intT :: Type
intT = ConT "Int"

realT :: Type
realT = ConT "Real"

stringT :: Type
stringT = ConT "String"

boolT :: Type
boolT = ConT "Bool"

isPrimType :: Type -> Bool
isPrimType (ConT c) = c `elem` ["Int", "Real", "String", "Bool"]
isPrimType _ = False

----------------------------------------
-- | Type schemes
----------------------------------------

data Scheme = Forall [Var] Type
  deriving (Show, Eq, Ord)

----------------------------------------
-- | Type environments
----------------------------------------

type TypeEnv = Map Var Scheme

emptyTypeEnv :: TypeEnv
emptyTypeEnv = Map.empty

-- | Extend a type environment with a new entry
extend :: TypeEnv -> Var -> Scheme -> TypeEnv
extend env v s = Map.insert v s env

-- | Case destructable types
isDestroyable :: TypeEnv -> Type -> Bool
isDestroyable _   t | isPrimType t = True
isDestroyable env t = case t of
  VarT {} -> True
  (:->:) {} -> False
  t1 :+: t2 -> isDestroyable env t1 && isDestroyable env t2
  TupT ts -> all (isDestroyable env) ts
  ConT c -> case Map.lookup c env of
    Just (Forall [] t') -> isDestroyable env t'
    _ -> False
