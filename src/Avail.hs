-- | @avail@ is a companion to monad transformers that allows you to add effect management to /concrete monads/,
-- i.e. specify what effects a piece of code can perform.
--
-- Traditionally, in order to manage effects, the /effect typeclasses/ are placed on a polymorphic monad type
-- @m@ so that other details of the monad type is not known at that point, effectively limiting what a function can do:
--
-- @
-- (MonadWriter Log m, MonadState Store m, MonadReader Env m) => m ()
-- @
--
-- While this works well, it has inevitable performance drawback because of the polymorphic @m@. GHC doesn't know the
-- implementation of @m@, hence cannot perform much optimization. On the other hand, if we use a concrete monad stack
-- that supports all the effects we need, we will not be able to restrict the effects that can be performed.
--
-- @avail@ addresses this by a monad transformer 'M'. For any monad @m@, the monad type @'M' m@ adds effect
-- management on top of it. Specifically, for an effect typeclass @c@ (such as 'Control.Monad.MonadIO' or
-- @'Control.Monad.Reader.MonadReader' r@), its methods can be used on @'M' m@ only if:
--
-- * The monad @m@ actually supports the effect, i.e. has an instance @c m@ of the effect typeclass;
-- * The effect is /available/ in current context, i.e. a /phantom constraint/ @'Eff' c@ (which doesn't contain any
-- information) is added to the function signature.
--
-- This pattern was first outlined in the blog post
-- [/Effect is a phantom/](https://喵.世界/2021/09/14/redundant-constraints/).
-- In @avail@, it allows you to manage effects via the phantom 'Eff' constraint while still using a
-- concrete monad stack; the 'Eff' constarint is not tied to the stack anyhow. Finally, 'Eff' has no instances,
-- and can only be removed all at once via the 'runM' function, obtaining the underlying monad.
--
-- @avail@ supports libraries including @mtl@, @unliftio@, @monad-control@ and @capability@ out of the box, so there
-- should be near-zero boilerplate to get started with @avail@. For other effect typeclasses, the @avail@ support
-- of them can be easily derived via the TH functions in "Avail.Derive".
--
-- You need these language extensions when using this module:
--
-- @
-- DataKinds
-- FlexibleContexts
-- FlexibleInstances
-- RankNTypes
-- TypeApplications
-- @
--
-- You need more extensions when using "Avail.Derive"; see documentation in that module.
module Avail
  ( M, Effect, IsEff (Superclasses), Eff, Effs, KnownList, unM, runM
  ) where

import           Avail.Instances ()
import           Avail.Internal
