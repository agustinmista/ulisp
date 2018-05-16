{-# LANGUAGE TypeSynonymInstances #-}

module Pretty
  ( Pretty(..)
  , pretty
  ) where

import Text.PrettyPrint

import Syntax

-- | Main pretty printing function
pretty :: Pretty a => a -> String
pretty = renderStyle (Style PageMode 80 2) . pp

-- | Type class for pretty printable types with an explicit level
class Pretty p where
  ppr :: Int -> p -> Doc

  pp, pp' :: p -> Doc
  pp = ppr 0
  pp' = ppr 1

----------------------------------------
-- | Variables
----------------------------------------

instance Pretty Var where
  ppr _ v = text (showVar v)

----------------------------------------
-- | Top level declarations
----------------------------------------

instance Pretty Decl where

  ppr _ (BindD bind)
    = pp bind

  ppr _ (SigD v t)
    = text "sig"
    <+> pp v
    <+> colon
    <+> pp t

  ppr _ (TypeD v t)
    = text "type"
    <+> pp v
    <+> equals
    <+> pp t

  ppr _ (InfixD f op v)
    = text (if f == L then "infixl" else "infixr")
    <+> pp op
    <+> equals
    <+> pp v

----------------------------------------
-- | Binds
----------------------------------------

instance Pretty Bind where

  -- | val binds
  ppr _ (ValB v (AsE e t))
    = text "val"
      <+> pp v
      <+> colon
      <+> pp t
      <+> equals
      <+> pp e

  ppr _ (ValB v e)
    = text "val"
      <+> pp v
      <+> equals
      <+> pp e

  -- | fun binds
  ppr _ (FunB f (FixE e)) =
    case viewLamBody e of
      AsE body t ->
        text "fun"
        <+> pp f
        <+> pprFixLamVars e
        <+> colon
        <+> pp t
        <+> equals
        <+> pp body

      body ->
        text "fun"
        <+> pp f
        <+> pprFixLamVars e
        <+> equals
        <+> pp body

  ppr _ _ = error "ppr: found an impossible value!"

----------------------------------------
-- | Expressions
----------------------------------------

instance Pretty Expr where

  -- | Variables
  ppr _ (VarE v)
    = pp v

  -- | Function application
  ppr p e@(AppE {})
    = parensIf (p > 0)
    $ pp' (viewFun e)
      <+> sep (map (\arg -> (pp' arg)) (viewArgs e))

  -- | Lambda abstractions
  ppr p e@(LamE {})
    = parensIf (p > 0)
    $ text "fn"
      <+> hsep (map pp (viewLamVars e))
      <+> text "=>"
      <+> pp (viewLamBody e)

  -- | Let expressions
  ppr p e@(LetE _ (LetE {}))
    = parensIf (p > 0)
    $ text "let"
      <+> vcat (map pp (viewLetBinds e))
    $+$ text "in"
      <+> pp (viewLetBody e)
      $+$ text "end"
  ppr p (LetE b e)
    = parensIf (p > 0)
    $ text "let"
      <+> ppr p b
      $+$ text "in"
        <+> pp e
        $+$ text "end"

  -- | Literals
  ppr _ (LitE l)
    = pp l

  -- | Infix operators
  ppr p (InfixE op e1 e2)
    = parensIf (p > 0)
    $ pp' e1
    <+> pp op
    <+> pp' e2

  -- | If then else expressions
  ppr p (IfE b th el)
    = parensIf (p > 0)
    $ text "if"
      <+> pp b
      $+$ text "then"
        <+> pp th
        $+$ text "else"
          <+> pp el

  -- | Case expressions
  ppr p (CaseE e alts)
    = parensIf (p > 0)
    $ text "case"
      <+> pp e
      <+> text "of"
      $+$ vcat (map (\alt -> text "|" <+> pp alt) alts)

  -- | Fixpoints
  ppr p (FixE e)
    = parensIf (p > 0)
    $ text "fix"
      <+> (pp' e)

  -- | Tuples
  ppr _ (TupE es)
    = parens
    $ sep (punctuate comma (map pp es))

  -- | Sum injections
  ppr p (SumE (Left l))
    = parensIf (p > 0)
    $ text  "<<"
    <+> pp' l
  ppr p (SumE (Right r))
    = parensIf (p > 0)
    $ text ">>"
    <+> pp' r

  -- | Type annotations
  ppr p (AsE e t)
    = parensIf (p > 0)
    $ pp e
    <+> colon
    <+> pp t


----------------------------------------
-- | Case alternatives
----------------------------------------

instance Pretty Alt where
  ppr _ (Alt pat e) = pp pat <+> text "=>" <+> pp e

----------------------------------------
-- | Patterns
----------------------------------------

instance Pretty Pat where
  ppr _ (LitP l) = pp l
  ppr _ (VarP v) = pp v
  ppr _ (TupP ps) = parens (sep (punctuate comma (map pp ps)))
  ppr _ (SumP (Left l)) = text "<<" <+> pp l
  ppr _ (SumP (Right r)) = text ">>" <+> pp r

----------------------------------------
-- | Literals
----------------------------------------

instance Pretty Literal where
  ppr _ (IntL n) = integer n
  ppr _ (RealL n) = double n
  ppr _ (StringL s) = text (show s)
  ppr _ (BoolL True) = text "true"
  ppr _ (BoolL False) = text "false"

----------------------------------------
-- | Types
----------------------------------------

instance Pretty Type where
  ppr _ (VarT v)
    = pp v

  ppr _ (ConT v)
    = pp v

  ppr p (t1 :->: t2)
    = parensIf (p > 0)
    $ ppr p t1
      <+> text "->"
      <+> pp' t2

  ppr _ (t1 :+: t2)
    = pp t1
    <+> text "+"
    <+> pp t2

  ppr _ (TupT ts)
    = parens
    $ sep (punctuate comma (map pp ts))

----------------------------------------
-- | Type schemes
----------------------------------------

instance Pretty Scheme where
  ppr _ (Forall tv t)
    = text "forall"
    <+> hsep (map pp tv)
    <+> colon
    <+> pp t

----------------------------------------
-- | Auxiliary functions
----------------------------------------

-- | Views for lambda terms
viewLamVars :: Expr -> [Var]
viewLamVars (LamE v e) = v : viewLamVars e
viewLamVars _ = []

pprFixLamVars :: Expr -> Doc
pprFixLamVars = hsep . map pp . tail . viewLamVars

viewLamBody :: Expr -> Expr
viewLamBody (LamE _ e) = viewLamBody e
viewLamBody x = x

-- | Views for function applications
viewApp :: Expr -> (Expr, [Expr])
viewApp (AppE e1 e2) = go e1 [e2]
  where
    go (AppE a b) xs = go a (b : xs)
    go f xs = (f, xs)
viewApp _ = error "viewApp: not an AppE"

viewArgs :: Expr -> [Expr]
viewArgs = snd . viewApp

viewFun :: Expr -> Expr
viewFun = fst . viewApp

-- | Views for let expressions
viewLet :: Expr -> ([Bind], Expr)
viewLet (LetE b e) = (b : bs, body)
  where
    (bs, body) = viewLet e
viewLet body = ([], body)

viewLetBinds :: Expr -> [Bind]
viewLetBinds = fst . viewLet

viewLetBody :: Expr -> Expr
viewLetBody = snd . viewLet

-- | Conditional parens combinator
parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id
