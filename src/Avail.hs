-- | @avail@ is a companion to monad transformers that allows you to use a concrete monad with effect management. In
-- traditional approaches to monad transformers, one usually writes:
--
-- @
-- (MonadWriter Log m, MonadState Store m, MonadReader Env m) => m ()
-- @
--
-- While this works well, it has inevitable performance drawback because of the polymorphic @m@. GHC doesn't know the
-- implementation of @m@, hence cannot perform much optimization. In contrast, if we use a concrete monad @MyM@ that
-- interprets all effects we need, we will not be able to restrict the effects that can be performed.
--
-- @avail@ addresses this by the /phantom constraint pattern/, where an effect r can be performed if and only if:
--
-- * The monad is /capable of/ interpreting this effect,
-- * The effect is /available/ in current context, i.e. a phantom constraint @'Eff' r@, which doesn't contain any
-- information, is in the context.
--
-- This pattern is outlined in the blog post
-- [/Effect is a phantom/](https://喵.世界/2021/09/14/redundant-constraints/).
-- In @avail@, the usage of this pattern allows you to control the effect via the phantom 'Eff' constraint, while
-- using a concrete monad, because the phantom constraint 'Eff' is not tied to the monad. 'Eff' has no instances, and
-- can only be removed all at once, obtaining the underlying monad, via the 'runM' function.
--
-- @avail@ already supports libraries including @mtl@, @unliftio@, @monad-control@ and @capability@ out of the box, so
-- there should be near-zero boilerplate to get started with @avail@. For other monad typeclasses, the @avail@ support
-- of them can be easily derived via the TH functions in "Avail.Derive".
--
-- You need these language extensions when using this module:
--
-- @
-- DataKinds
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
