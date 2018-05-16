{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Pass where

import Data.Bifunctor
import Lens.Micro
import Lens.Micro.TH

import Syntax

----------------------------------------
-- | Algebras
----------------------------------------

-- | Variables
data VarAlg var = VarAlg
  { _var' :: Var -> var }

-- | Binds
data BindAlg var bind expr = BindAlg
  { _valB :: var -> expr -> bind
  , _funB :: var -> expr -> bind
  }

-- | Expressions
data ExprAlg var bind expr lit alt ty = ExprAlg
  { _varE :: var -> expr
  , _appE :: expr -> expr -> expr
  , _lamE :: var -> expr -> expr
  , _letE :: bind -> expr -> expr
  , _litE :: lit -> expr
  , _infixE :: var -> expr -> expr -> expr
  , _ifE :: expr -> expr -> expr -> expr
  , _caseE :: expr -> [alt] -> expr
  , _fixE :: expr -> expr
  , _tupE :: [expr] -> expr
  , _sumE :: Either expr expr -> expr
  , _asE :: expr -> ty -> expr
  }

-- | Literals
data LiteralAlg lit = LiteralAlg
  { _intL :: Integer -> lit
  , _realL :: Double -> lit
  , _stringL :: Text -> lit
  , _boolL :: Bool -> lit
  }

-- | Case alternatives
data AltAlg var expr alt pat = AltAlg
  { _alt' :: pat -> expr -> alt }

-- | Patterns
data PatAlg var lit pat = PatAlg
  { _litP :: lit -> pat
  , _varP :: var -> pat
  , _tupP :: [var] -> pat
  , _sumP :: Either var var -> pat
  }

-- | Types
data TypeAlg var ty = TypeAlg
  { _varT :: var -> ty
  , _conT :: var -> ty
  , _arrT :: ty -> ty -> ty
  , _sumT :: ty -> ty -> ty
  , _tupT :: [ty] -> ty
  }

-- | Dictionary of algebras
data AlgDict var bind expr lit alt pat ty = AlgDict
  { _var     :: VarAlg var
  , _bind    :: BindAlg var bind expr
  , _expr    :: ExprAlg var bind expr lit alt ty
  , _alt     :: AltAlg  var expr alt pat
  , _literal :: LiteralAlg lit
  , _pat     :: PatAlg var lit pat
  , _type'   :: TypeAlg var ty
  }

makeLenses ''VarAlg
makeLenses ''BindAlg
makeLenses ''ExprAlg
makeLenses ''LiteralAlg
makeLenses ''AltAlg
makeLenses ''PatAlg
makeLenses ''TypeAlg
makeLenses ''AlgDict

----------------------------------------
-- | Folds
----------------------------------------

foldVar :: AlgDict var bind expr lit alt pat ty -> Var -> var
foldVar dict v = _var' (_var dict) v

foldBind :: AlgDict var bind expr lit alt pat ty -> Bind -> bind
foldBind dict = \case
  ValB v e -> _valB alg (fv v) (fe e)
  FunB v e -> _funB alg (fv v) (fe e)
  where
    alg = _bind dict
    fv = foldVar dict
    fe = foldExpr dict

foldExpr :: AlgDict var bind expr lit alt pat ty -> Expr -> expr
foldExpr dict = \case
  VarE v -> _varE alg (fv v)
  AppE a b -> _appE alg (fe a) (fe b)
  LamE v e -> _lamE alg (fv v) (fe e)
  LetE b e -> _letE alg (fb b) (fe e)
  LitE l -> _litE alg (fl l)
  InfixE op e1 e2 -> _infixE alg (fv op) (fe e1) (fe e2)
  IfE c th el -> _ifE alg (fe c) (fe th) (fe el)
  CaseE e alts -> _caseE alg (fe e) (map fa alts)
  FixE e -> _fixE alg (fe e)
  TupE es -> _tupE alg (map fe es)
  SumE s -> _sumE alg (bimap fe fe s)
  AsE e t -> _asE alg (fe e) (ft t)
  where
    alg = _expr dict
    fv = foldVar dict
    fb = foldBind dict
    fe = foldExpr dict
    fl = foldLiteral dict
    fa = foldAlt dict
    ft = foldType dict

foldLiteral :: AlgDict var bind expr lit alt pat ty -> Literal -> lit
foldLiteral dict = \case
  IntL n -> _intL alg n
  RealL n -> _realL alg n
  StringL s -> _stringL alg s
  BoolL b -> _boolL alg b
  where
    alg = _literal dict

foldAlt :: AlgDict var bind expr lit alt pat ty -> Alt -> alt
foldAlt dict (Alt p e) = _alt' alg (fp p) (fe e)
  where
    alg = _alt dict
    fe = foldExpr dict
    fp = foldPat dict

foldPat :: AlgDict var bind expr lit alt pat ty -> Pat -> pat
foldPat dict = \case
  LitP l -> _litP alg (fl l)
  VarP v -> _varP alg (fv v)
  TupP ps -> _tupP alg (map fv ps)
  SumP s -> _sumP alg (bimap fv fv s)
  where
    alg = _pat dict
    fv = foldVar dict
    fl = foldLiteral dict

foldType :: AlgDict var bind expr lit alt pat ty -> Type -> ty
foldType dict = \case
  VarT v -> _varT alg (fv v)
  ConT v -> _conT alg (fv v)
  t1 :->: t2 -> _arrT alg (ft t1) (ft t2)
  t1 :+: t2 -> _sumT alg (ft t1) (ft t2)
  TupT ts -> _tupT alg (map ft ts)
  where
    alg = _type' dict
    fv = foldVar dict
    ft = foldType dict

----------------------------------------
-- | Endomorphic passes
----------------------------------------

type PassDict = AlgDict Var Bind Expr Literal Alt Pat Type

defaultPassDict :: PassDict
defaultPassDict = AlgDict
  { _var = VarAlg
    { _var' = id }
  , _bind = BindAlg
    { _valB = ValB, _funB = FunB }
  , _expr = ExprAlg
    { _varE = VarE, _appE = AppE, _lamE = LamE, _letE = LetE
    , _litE = LitE, _infixE = InfixE, _ifE = IfE, _caseE = CaseE
    , _fixE = FixE, _tupE = TupE, _sumE = SumE, _asE = AsE }
  , _literal = LiteralAlg
    { _intL = IntL, _realL = RealL, _stringL = StringL, _boolL = BoolL }
  , _alt = AltAlg
    { _alt' = Alt }
  , _pat = PatAlg
    { _litP = LitP, _varP = VarP, _tupP = TupP, _sumP = SumP}
  , _type' = TypeAlg
    { _varT = VarT, _conT = ConT
    , _arrT = (:->:), _sumT = (:+:), _tupT = TupT }
  }


class PassOver a where
  passOver :: forall x y . ASetter PassDict PassDict x y -> y -> a -> a

instance PassOver Var where
  passOver setter f = foldVar (set setter f defaultPassDict)

instance PassOver Bind where
  passOver setter f = foldBind (set setter f defaultPassDict)

instance PassOver Expr where
  passOver setter f = foldExpr (set setter f defaultPassDict)

instance PassOver Alt where
  passOver setter f = foldAlt (set setter f defaultPassDict)

instance PassOver Literal where
  passOver setter f = foldLiteral (set setter f defaultPassDict)

instance PassOver Pat where
  passOver setter f = foldPat (set setter f defaultPassDict)


-- | A generic pass over any type of the AST
type Pass = forall a . PassOver a => a -> a

-- duplicateInts :: Pass
-- duplicateInts = passOver (literal . intL) (IntL . (*2))

-- revArrows :: Pass
-- revArrows = passOver (type' . arrT) (flip ArrT)
