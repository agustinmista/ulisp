{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infer where

import Syntax

import Control.Monad.State
import Control.Monad.Except

import Data.List
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set


-- | Type errors
data TypeError
  = UnificationFail Type Type
  | InfiniteType Var Type
  | UnboundVariable String
  deriving Show

-- | Unique identifiers
type Unique = Int

initUnique :: Unique
initUnique = 0

-- | Type substitutions
type Subst = Map Var Type

nullSubst :: Subst
nullSubst = Map.empty

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set Var

instance Substitutable Type where
  apply s (VarT v) = Map.findWithDefault (VarT v) v s
  apply _ (ConT c) = ConT c
  apply s (t1 :->: t2) = apply s t1 :->: apply s t2
  apply s (t1 :+: t2) = apply s t1 :+: apply s t2
  apply s (TupT ts) = TupT (map (apply s) ts)

  ftv (VarT v) = Set.singleton v
  ftv (ConT _) = Set.empty
  ftv (t1 :->: t2) = Set.union (ftv t1) (ftv t2)
  ftv (t1 :+: t2) = Set.union (ftv t1) (ftv t2)
  ftv (TupT ts) = Set.unions (map ftv ts)

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as (apply (foldr Map.delete s as) t)
  ftv (Forall as t) = Set.difference (ftv t) (Set.fromList as)

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv   = Set.unions . map ftv

instance Substitutable TypeEnv where
  apply s env =  Map.map (apply s) env
  ftv env = ftv (Map.elems env)

compose :: [Subst] -> Subst
compose = foldl comp nullSubst
  where comp s1 s2 = Map.union (Map.map (apply s1) s2) s1

-- | Inference monad
type Infer a = ExceptT TypeError (State Unique) a

-- | Fresh type vars generation
fresh :: Infer Type
fresh = do
  s <- get
  put (s + 1)
  return (VarT (letters !! s))

-- | Infinite type vars supply
letters :: [Text]
letters = map mkVar (flip replicateM ['a'..'z'] =<< [1..])

-- | Type unification
unify :: Type -> Type -> Infer Subst
unify (VarT v) t = bind v t
unify t (VarT v) = bind v t
unify (ConT c) (ConT c')
  | c == c' = pure nullSubst
unify (t1 :->: t2) (t1' :->: t2') = do
  s1 <- unify t1 t1'
  s2 <- unify (apply s1 t2) (apply s1 t2')
  return (compose [s2, s1])
unify (t1 :+: t2) (t1' :+: t2') = do
  s1 <- unify t1 t1'
  s2 <- unify (apply s1 t2) (apply s1 t2')
  return (compose [s2, s1])
unify (TupT ts) (TupT ts')
  | length ts == length ts' = do
      ss <- mapM (uncurry unify) (zip ts ts')
      return (compose ss)
unify t1 t2 = throwError (UnificationFail t1 t2)

bind :: Var -> Type -> Infer Subst
bind v t
  | t == VarT v = pure nullSubst
  | Set.member v (ftv t) = throwError (InfiniteType v t)
  | otherwise = pure (Map.singleton v t)

lookupEnv :: TypeEnv -> Var -> Infer (Subst, Type)
lookupEnv env v = case Map.lookup v env of
    Nothing -> throwError (UnboundVariable (showVar v))
    Just s  -> do
      t <- instantiate s
      return (nullSubst, t)

-- | Type instantiation
instantiate :: Scheme -> Infer Type
instantiate (Forall vs t) = do
  vs' <- mapM (const fresh) vs
  let s = Map.fromList (zip vs vs')
  return (apply s t)

-- | Type generalization
generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall vs t
  where
    vs = Set.toList (Set.difference (ftv t) (ftv env))

-- | Type inference
infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env = \case
  VarE v -> lookupEnv env v

  AppE e1 e2 -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3 <- unify (apply s2 t1) (t2 :->: tv)
    return (compose [s3, s2, s1], apply s3 tv)

  LamE x e -> do
    tv <- fresh
    let env' = extend env x (Forall [] tv)
    (s1, t1) <- infer env' e
    return (s1, apply s1 tv :->: t1)

  LetE b body -> do
    let (v, e) = getBind b
    (s1, t1) <- infer env e
    let env' = apply s1 env
        t'   = generalize env' t1
    (s2, t2) <- infer (extend env' v t') body
    return (compose [s2, s1], t2)

  LitE (IntL _)    -> return (nullSubst, intT)
  LitE (RealL _)   -> return (nullSubst, realT)
  LitE (StringL _) -> return (nullSubst, stringT)
  LitE (BoolL _)   -> return (nullSubst, boolT)

  InfixE op e1 e2 -> do
    (s1, _) <- lookupEnv env op
    (s2, t) <- infer env (AppE (AppE (VarE op) e1) e2)
    return (compose [s2, s1], t)

  IfE c th el -> do
    (s1, t1) <- infer env c
    (s2, t2) <- infer env th
    (s3, t3) <- infer env el
    s4 <- unify t1 boolT
    s5 <- unify t2 t3
    return (compose [s5, s4, s3, s2, s1], apply s5 t2)

  CaseE _ _ -> error "infer: caseE not yet implemented"

  FixE e -> do
    tv <- fresh
    (s1, t) <- infer env e
    s2 <- unify (tv :->: tv) t
    return (s2, apply s1 tv)

  TupE es -> do
    infs <- mapM (infer env) es
    let (ss, ts) = unzip infs
    return (compose ss, TupT ts)

  SumE (Left el) -> do
    tv <- fresh
    (s1, tl) <- infer env el
    return (s1, tl :+: tv)
  SumE (Right er) -> do
    tv <- fresh
    (s1, tr) <- infer env er
    return (s1, tv :+: tr)

  AsE e t -> do
    (s1, t1) <- infer env e
    s2 <- unify (apply s1 t1) t
    return (compose [s2, s1], t)


runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err  -> Left err
  Right res -> Right (closeOver res)

closeOver :: (Map Var Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where sc = generalize emptyTypeEnv (apply sub ty)

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (fmap snd ord) (normtype body)
  where
    ord = zip (nub (fv body)) letters

    fv (VarT a)     = [a]
    fv (ConT _)     = []
    fv (t1 :->: t2) = fv t1 ++ fv t2
    fv (t1 :+: t2)  = fv t1 ++ fv t2
    fv (TupT ts)    = concat (map fv ts)

    normtype (VarT v)     = maybe (error (varTError v)) VarT (lookup v ord)
    normtype (ConT a)     = ConT a
    normtype (t1 :->: t2) = normtype t1 :->: normtype t2
    normtype (t1 :+: t2)  = normtype t1 :+: normtype t2
    normtype (TupT ts)    = TupT (map normtype ts)

    varTError v = "normalize: type variable " ++ show v ++ " not in signature"

-- patternType :: Pat -> Infer Scheme
-- patternType = \case
--   LitP (IntL    _) -> return (Forall [] intT)
--   LitP (RealL   _) -> return (Forall [] realT)
--   LitP (StringL _) -> return (Forall [] stringT)
--   LitP (BoolL   _) -> return (Forall [] boolT)
--   VarP v           -> return (Forall [v] (VarT v))
--   SumP (Left l) -> do
--     VarT r <- fresh
--     return (Forall [r] (VarT l :+: VarT r))
--   SumP (Right r) -> do
--     VarT l <- fresh
--     return (Forall [r] (VarT l :+: VarT r))


inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env = runInfer . infer env

typeof :: TypeEnv -> Var -> Maybe Scheme
typeof env name = Map.lookup name env
