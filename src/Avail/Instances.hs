-- | This module exports instances of the 'M' monad for all typeclasses in @capability@, @transformers-base@,
-- @monad-control@, @exceptions@, @unliftio@ and @mtl@.
--
-- All instances are reexported in the "Avail" module, so you don't need to import this module.
{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Avail.Instances () where

import           Avail.Derive
import           Avail.Internal
import           Control.Applicative         (Alternative)
import           Control.Monad               (MonadPlus)
import           Control.Monad.Fail          (MonadFail)
import           Control.Monad.IO.Class      (MonadIO)
import           Prelude                     hiding (MonadFail)

#ifdef AVAIL_exceptions
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
#endif

#ifdef AVAIL_mtl
import           Control.Monad.Cont          (MonadCont)
import           Control.Monad.Except        (MonadError)
import           Control.Monad.RWS           (MonadRWS)
import           Control.Monad.Reader        (MonadReader)
import           Control.Monad.State         (MonadState)
import           Control.Monad.Writer        (MonadWriter)
#endif

#ifdef AVAIL_semigroupoids
import           Data.Functor.Alt            (Alt ((<!>)))
import           Data.Functor.Plus           (Plus)
#endif

#ifdef AVAIL_unliftio
import           Control.Monad.IO.Unlift     (MonadUnliftIO)
#endif

#ifdef AVAIL_monad_control
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Trans.Control (MonadBaseControl)
#endif

#ifdef AVAIL_primitive
import           Control.Monad.Primitive     (PrimMonad)
#endif

#ifdef AVAIL_capability
import           Capability.Error            (HasCatch, HasThrow)
import           Capability.Reader           (HasReader)
import           Capability.Sink             (HasSink)
import           Capability.Source           (HasSource)
import           Capability.State            (HasState)
import           Capability.Writer           (HasWriter)
#endif

avail [t| MonadIO |]
avail [t| MonadFail |]
avail [t| Alternative |]
avail' [[t| Alternative |]] [t| MonadPlus |]

#ifdef AVAIL_exceptions
avail [t| MonadThrow |]
avail' [[t| MonadThrow |]] [t| MonadCatch |]
avail' [[t| MonadCatch |]] [t| MonadMask |]
#endif

#ifdef AVAIL_mtl
avail [t| MonadCont |]
with1' "r" $ \r -> avail [t| MonadReader $r |]
with1' "w" $ \w -> avail [t| MonadWriter $w |]
with1' "s" $ \s -> avail [t| MonadState $s |]
with1' "e" $ \e -> avail [t| MonadError $e |]
with3' "r" "w" "s" $ \r w s -> avail'
  [ [t| MonadReader $r |]
  , [t| MonadWriter $w |]
  , [t| MonadState $s |]
  ] [t| MonadRWS $r $w $s |]
#endif

#ifdef AVAIL_semigroupoids
avail [t| Plus |]
instance IsEff Alt where
  type Superclasses Alt = '[]
instance Alt m => Alt (M m) where
  UnsafeLift m <!> UnsafeLift n = UnsafeLift $ m <!> n
#endif

#ifdef AVAIL_unliftio
avail' [[t| MonadIO |]] [t| MonadUnliftIO |]
#endif

#ifdef AVAIL_monad_control
with1' "b" $ \b -> avail [t| MonadBase $b |]
with1' "b" $ \b -> avail' [[t| MonadBase $b |]] [t| MonadBaseControl $b |]
#endif

#ifdef AVAIL_primitive
avail [t| PrimMonad |]
#endif

#ifdef AVAIL_capability
with2' "tag" "a" $ \tag a -> avail [t| HasSource $tag $a |]
with2' "tag" "a" $ \tag a -> avail [t| HasSink $tag $a |]
with2' "tag" "e" $ \tag e -> avail [t| HasThrow $tag $e |]

with2' "tag" "r" $ \tag r -> avail' [[t| HasSource $tag $r |]] [t| HasReader $tag $r |]
with2' "tag" "w" $ \tag w -> avail' [[t| HasSink $tag $w |]] [t| HasWriter $tag $w |]
with2' "tag" "e" $ \tag e -> avail' [[t| HasThrow $tag $e |]] [t| HasCatch $tag $e |]

with2' "tag" "s" $ \tag s -> avail'
  [ [t| HasSource $tag $s |]
  , [t| HasSink $tag $s |]
  ] [t| HasState $tag $s |]
#endif
