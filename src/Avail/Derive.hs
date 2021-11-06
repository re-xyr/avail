{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- | This module contains mechanisms for deriving necessary instances for a new 'Effect' typeclass to work with
-- @avail@. If you only need functionalities from @mtl@, @monad-control@, @unliftio@ and @capability@, you don't need
-- to use this module.
module Avail.Derive
  ( -- * Deriving
    avail, avail'
  , -- * Helpers for deriving instances for multi-param classes
    with1, with2, with3, with4, with5, withN,
    with1', with2', with3', with4', with5', withN'
  , -- * Necessary reexports - do not use directly
    M (UnsafeLift)
  ) where

import           Avail.Internal
import           Data.Kind           (Type)
import           Language.Haskell.TH hiding (Type)
import qualified Language.Haskell.TH as TH

-- | Derive necessary instances for an 'Effect' typeclass to work with @avail@. Specifically, this only works with
-- typeclasses without superclasses; see 'avail'' for a version that considers superclasses.
avail :: Q TH.Type -> Q [Dec]
avail = avail' []

-- | Derive necessary instances for an 'Effect' typeclass to work with @avail@. This is a generalized version of
-- 'avail' that allows you to pass in a list of superclasses.
--
-- For superclasses @Sup :: ['Effect']@ and current class @Cls :: 'Effect'@, the code generated is:
--
-- @
-- instance 'IsEff' Cls where
--   type 'Superclasses' Cls = Sup
-- deriving via (m :: 'Type' -> 'Type') instance (Cls m, 'Eff' Cls) => Cls ('M' m)
-- @
avail' :: [Q TH.Type] -> Q TH.Type -> Q [Dec]
avail' = avail'' $ \m -> [t| M $m |]

avail'' :: (Q TH.Type -> Q TH.Type) -> [Q TH.Type] -> Q TH.Type -> Q [Dec]
avail'' mm pre cls = do
  m <- newName "m"
  let mTy = pure $ VarT m
  let mmTy = mm mTy
  isEff <- [d|
    instance IsEff $cls where
      type Superclasses $cls = $(makeList <$> sequence pre) |]
  deriv <- StandaloneDerivD
    <$> (Just . ViaStrategy <$> [t| $mTy :: Type -> Type |])
    <*> sequence [[t| $cls $mTy |], [t| Eff $cls |]]
    <*> [t| $cls $mmTy |]
  pure (deriv : isEff)
  where
    makeList []       = PromotedNilT
    makeList (x : xs) = PromotedConsT `AppT` x `AppT` makeList xs

-- | Introduce one type variable @a@.
with1 :: (Q TH.Type -> Q a) -> Q a
with1 = with1' "a"

-- | Introduce one type variable with given name.
with1' :: String -> (Q TH.Type -> Q a) -> Q a
with1' n f = withN' [n] (\[a] -> f a)

-- | Introduce two type variables @a, b@.
with2 :: (Q TH.Type -> Q TH.Type -> Q a) -> Q a
with2 = with2' "a" "b"

-- | Introduce two type variables with given names.
with2' :: String -> String -> (Q TH.Type -> Q TH.Type -> Q a) -> Q a
with2' n1 n2 f = withN' [n1, n2] (\[a, b] -> f a b)

-- | Introduce three type variables @a, b, c@.
with3 :: (Q TH.Type -> Q TH.Type -> Q TH.Type -> Q a) -> Q a
with3 = with3' "a" "b" "c"

-- | Introduce three type variables with given names.
with3' :: String -> String -> String -> (Q TH.Type -> Q TH.Type -> Q TH.Type -> Q a) -> Q a
with3' n1 n2 n3 f = withN' [n1, n2, n3] (\[a, b, c] -> f a b c)

-- | Introduce four type variables @a, b, c, d@.
with4 :: (Q TH.Type -> Q TH.Type -> Q TH.Type -> Q TH.Type -> Q a) -> Q a
with4 = with4' "a" "b" "c" "d"

-- | Introduce four type variables with given names.
with4' :: String -> String -> String -> String -> (Q TH.Type -> Q TH.Type -> Q TH.Type -> Q TH.Type -> Q a) -> Q a
with4' n1 n2 n3 n4 f = withN' [n1, n2, n3, n4] (\[a, b, c, d] -> f a b c d)

-- | Introduce five type variables @a, b, c, d, e@.
with5 :: (Q TH.Type -> Q TH.Type -> Q TH.Type -> Q TH.Type -> Q TH.Type -> Q a) -> Q a
with5 = with5' "a" "b" "c" "d" "e"

-- | Introduce five type variables with given names.
with5' :: String -> String -> String -> String -> String -> (Q TH.Type -> Q TH.Type -> Q TH.Type -> Q TH.Type -> Q TH.Type -> Q a) -> Q a
with5' n1 n2 n3 n4 n5 f = withN' [n1, n2, n3, n4, n5] (\[a, b, c, d, e] -> f a b c d e)

-- | Introduce arbitrarily many type variables @a1, a2, a3, ...@.
withN :: Int -> ([Q TH.Type] -> Q a) -> Q a
withN n = withN' $ ('a' :) . show <$> [1..n]

-- | Introduce arbitrarily many type variables with given names.
withN' :: [String] -> ([Q TH.Type] -> Q a) -> Q a
withN' n f = do
  as <- traverse (fmap VarT . newName) n
  f (pure <$> as)
