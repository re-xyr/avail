-- | This module contains mechanisms for deriving necessary instances for a new 'Effect' typeclass to work with
-- @avail@. If you only need functionalities from @mtl@, @monad-control@, @unliftio@ and @capability@, you don't need
-- to use this module.
module Avail.Derive
  ( -- * Deriving
    avail, avail'
  , -- * Helpers for deriving instances for multi-param classes
    with1, with2, with3, with4, with5, withN
  , -- * Necessary reexports - do not use directly
    M (UnsafeLift)
  ) where

import           Avail.Internal
import           Control.Monad       (replicateM)
import           Data.Bifunctor      (second)
import           Data.Kind           (Type)
import           Language.Haskell.TH hiding (Type)
import qualified Language.Haskell.TH as TH

-- | Derive necessary instances for an 'Effect' typeclass to work with @avail@. Specifically, this only works with
-- typeclasses without superclasses; see 'avail'' for a version that considers superclasses.
avail :: Q TH.Type -> Q [Dec]
avail = avail' []

-- | Derive necessary instances for an 'Effect' typeclass to work with @avail@. This is a generalized version of
-- 'avail' that allows you to pass in a list of superclasses.
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
    <*> sequence ([t| $cls $mTy |] : [t| Eff' $cls |] : ((>>= (\c -> [t| Eff $(pure c) |])) <$> pre))
    <*> [t| $cls $mmTy |]
  pure (deriv : isEff)
  where
    makeList []       = PromotedNilT
    makeList (x : xs) = PromotedConsT `AppT` x `AppT` makeList xs

-- | Introduce one type variable.
with1 :: (Q TH.Type -> Q a) -> Q a
with1 f = withN 1 (\[a] -> f a)

-- | Introduce two type variables.
with2 :: (Q TH.Type -> Q TH.Type -> Q a) -> Q a
with2 f = withN 2 (\[a, b] -> f a b)

-- | Introduce three type variables.
with3 :: (Q TH.Type -> Q TH.Type -> Q TH.Type -> Q a) -> Q a
with3 f = withN 3 (\[a, b, c] -> f a b c)

-- | Introduce four type variables.
with4 :: (Q TH.Type -> Q TH.Type -> Q TH.Type -> Q TH.Type -> Q a) -> Q a
with4 f = withN 4 (\[a, b, c, d] -> f a b c d)

-- | Introduce five type variables.
with5 :: (Q TH.Type -> Q TH.Type -> Q TH.Type -> Q TH.Type ->Q TH.Type -> Q a) -> Q a
with5 f = withN 5 (\[a, b, c, d, e] -> f a b c d e)

-- | Introduce arbitrarily many type variables.
withN :: Int -> ([Q TH.Type] -> Q a) -> Q a
withN n f = do
  as <- replicateM n (VarT <$> newName "a")
  f (pure <$> as)
