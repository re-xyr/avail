-- | This module defines the 'M' wrapper monad and the 'Eff' phantom constraint. All safe functionalities in this
-- module are reexported in the "Avail" module, so you wouldn't need to import this module most of the times.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK not-home #-}
module Avail.Internal where

import           Control.Monad.Fix (MonadFix)
import           Control.Monad.Zip (MonadZip)
import           Data.Kind         (Constraint, Type)
import           Data.Proxy        (Proxy (Proxy))
import           Unsafe.Coerce     (unsafeCoerce)

-- | The 'M' monad transformer acts as a /barrier of effects/. For example, for a monad type @App@ and any
-- effect typeclass @MonadOvO@ that @App@ has an instance of, the constraint @Eff MonadOvO@ is required to perform
-- the methods of @MonadOvO@ in the monad @'M' App@ as defined for the @App@ monad.
--
-- In particular, 'M' is expected to be used on a __concrete__ monad instead of a /polymorphic/ one. This is
-- particularly good in terms of program performance, and generally means instead of writing this:
--
-- @
-- f :: 'Control.Monad.State.MonadState' 'Int' m => m ()
-- @
--
-- You should write
--
-- @
-- f :: 'Eff' ('Control.Monad.State.MonadState' 'Int') => 'M' App ()
-- @
--
-- where @App@ is a monad stack of your choice that has support of @'Control.Monad.State.MonadState' 'Int'@. This also
-- means there is no 'Control.Monad.Trans.Class.MonadTrans' instance for 'M'.
--
-- Note: __you should not define instances of 'M' for effect typeclasses directly by hand__ as that is error-prone
-- and may create holes in effect management. For defining instances of effect typeclasses for 'M', check out
-- the "Avail.Derive" module and specifically the 'Avail.Derive.avail' and 'Avail.Derive.avail'' TH functions.
--
-- Also keep in mind that typeclasses inside @mtl@, @exceptions@, @unliftio@, @monad-control@ and @capability@ work
-- with 'M' out-of-the-box so no instance for them is needed to be defined on 'M' /by you/.
newtype M m a = UnsafeLift (m a) -- ^ Unsafely lift an @m@ action into @'M' m@. This completely sidesteps the
                                 -- effect management mechanism; __You should not use this.__
  deriving newtype (Functor, Applicative, Monad, MonadFix, MonadZip)

-- | The kind of /effect typeclasses/, i.e. those that define a set of operations on a monad. Examples include
-- 'Control.Monad.IO.Class.MonadIO' and 'Control.Monad.Reader.MonadReader'.
--
-- This type is the same as the 'Capability.Constraints.Capability' type in @capability@.
type Effect = (Type -> Type) -> Constraint

-- | Any 'Effect' being used with @avail@ should have an instance of this class. Specifically, this class stores
-- the /superclasses/ of effect typeclasses. For example, 'Control.Monad.IO.Unlift.MonadUnliftIO' has a superclass
-- 'Control.Monad.IO.Class.MonadIO'.
--
-- You won't need to define instances of this by hand; instead, use the 'Avail.Derive.avail'' Template Haskell function.
class KnownList (Superclasses e) => IsEff (e :: Effect) where
  -- | The superclasses of this typeclass.
  type Superclasses e :: [Effect]

-- | The /primitive/ phantom effect constraint that does not take superclasses into account. You should not use this
-- directly; use 'Eff' or 'Effs' instead. Additionally, you definitely shouldn't define instances for this class.
class Eff' (e :: Effect) where
  -- | The dummy method of the phantom typeclass, to be instantiated via the reflection trick in 'rip''.
  instEffect :: Proxy e
  instEffect = error "unimplemented"

-- | The constraint that indicates an effect is available for use, i.e. you can perform methods defined by instances
-- of the effect typeclass @e@ in a 'M' monad.
type Eff (e :: Effect) = (Eff' e, Effs (Superclasses e))

-- | Convenient alias for @('Eff' e1, 'Eff' e2, ..., 'Eff' en)@.
type family Effs (es :: [Effect]) :: Constraint where
  Effs '[] = ()
  Effs (e ': es) = (Eff e, Effs es)

-- | The newtype wrapper used to circumvent the impredicative problem of GHC and perform the reflection trick in
-- 'rip''. You have no reason to use this directly.
newtype InstEff e a = InstEff (Eff' e => a)

-- | Brutally rip off an 'Eff'' constraint, a la
-- [the reflection trick](https://hackage.haskell.org/package/base-4.16.0.0/docs/Unsafe-Coerce.html#v:unsafeCoerce).
-- __This is highly unsafe__ in terms of effect management.
rip' :: forall e a. (Eff' e => a) -> a
rip' x = unsafeCoerce (InstEff @e x) Proxy

-- | Brutally rip off an 'Eff' constraint. This means 'rip''ing off the 'Eff'' constraint of the current 'Effect'
-- and then 'rips' off constraints of all 'Superclasses' recursively. __This is highly unsafe__ in terms of effect
-- management.
rip :: forall e a. IsEff e => (Eff e => a) -> a
rip x = rips @(Superclasses e) $ rip' @e x

-- | The list of effect typeclasses @es@ is known at compile time. This is required for functions like 'runM'.
class KnownList (es :: [Effect]) where
  -- | Brutally rip off many 'Eff' constraints. __This is highly unsafe__ in terms of effect management.
  rips :: (Effs es => a) -> a
  rips _ = error "unimplemented"

instance KnownList '[] where
  rips = id

instance (IsEff e, KnownList es) => KnownList (e ': es) where
  rips x = rips @es $ rip @e x

-- | Unwrap the 'M' monad into the underlying concrete monad. This is rarely needed as most of the time you would also
-- want to eliminate 'Eff' constraints at the same time; for that see 'runM'.
unM :: M m a -> m a
unM (UnsafeLift m) = m

-- | Unwrap the 'M' monad into the underlying concrete monad and also eliminating 'Eff' constraints. You need
-- @TypeApplications@ in order to specify the list of 'Effect's you want to eliminate 'Eff' constraints for:
--
-- @
-- 'runM' @'[MonadReader Env, MonadState Store, MonadError MyErr] app
-- @
--
-- Note that functions like '(Data.Function.&)' generally does not work with this function; either apply directly or
-- use '($)' only.
runM :: forall es m a. KnownList es => (Effs es => M m a) -> m a
runM m = rips @es $ unM m
