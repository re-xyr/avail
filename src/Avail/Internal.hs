{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK not-home #-}
module Avail.Internal where

import           Data.Kind     (Constraint, Type)
import           Data.Proxy    (Proxy (Proxy))
import           Unsafe.Coerce (unsafeCoerce)

-- | To restrict the effects that can be performed, this monad is used to wrap the concrete monad @m@ everywhere. Users
-- are therefore screened from directly manipulating the underlying monad @m@, and any effect operation on @m@ will
-- require the 'Eff' constraint.
newtype M m a = UnsafeLift (m a) -- ^ Unsafely lift an @m@ action into @'M' m@. You should not use this.
  deriving newtype (Semigroup, Monoid, Functor, Applicative, Monad)

-- | The kind of effect typeclasses, i.e. those that define a set of operations on a monad. Examples include
-- 'Control.Monad.IO.Class.MonadIO' and 'Control.Monad.Reader.MonadReader'.
type Effect = (Type -> Type) -> Constraint

-- | Any 'Effect' being used with @avail@ should have an instance of this class. Specifically, this class stores the
-- /superclasses/ of the effect. For example, 'Control.Monad.IO.Unlift.MonadUnliftIO' has a superclass
-- 'Control.Monad.IO.Class.MonadIO'.
--
-- You won't need to define instances of this by hand; instead, use the 'Avail.Derive.avail'' Template Haskell
-- function.
class KnownList (Superclasses e) => IsEff (e :: Effect) where
  type Superclasses e :: [Effect]

class IsEff e => Eff' (e :: Effect) where
  instEffect :: Proxy e
  instEffect = error "unimplemented"

-- | The constraint that indicates an effect is available to use, i.e. you can perform effect operations defined by the
-- typeclass 'e'.
type Eff (e :: Effect) = (Eff' e, Effs (Superclasses e))

-- | Convenient alias for @('Eff' e1, 'Eff' e2, ..., 'Eff' en)@.
type family Effs (es :: [Effect]) :: Constraint where
  Effs '[] = ()
  Effs (e ': es) = (Eff e, Effs es)

newtype InstEff e a = InstEff (Eff' e => a)

rip' :: forall e a. (Eff' e => a) -> a
rip' x = unsafeCoerce (InstEff @e x) Proxy

rip :: forall e a. IsEff e => (Eff e => a) -> a
rip x = rips @(Superclasses e) $ rip' @e x

-- | The list of effects @es@ is known at compile time. This is required for functions like 'runM'.
class KnownList (es :: [Effect]) where
  rips :: (Effs es => a) -> a
  rips _ = error "unimplemented"

instance KnownList '[] where
  rips = id

instance (IsEff e, KnownList es) => KnownList (e ': es) where
  rips x = rips @es $ rip @e x

-- | Unwrap the 'M' monad into the underlying concrete monad.
unM :: M m a -> m a
unM (UnsafeLift m) = m

-- | Unwrap the 'M' monad into the underlying concrete monad, eliminating 'Eff' constraints.
runM :: forall es m a. KnownList es => (Effs es => M m a) -> m a
runM m = rips @es $ unM m
