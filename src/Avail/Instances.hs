{-# LANGUAGE UnboxedTuples #-}
module Avail.Instances () where

import           Avail.Derive
import           Capability.Error            (HasCatch, HasThrow)
import           Capability.Reader           (HasReader)
import           Capability.Sink             (HasSink)
import           Capability.Source           (HasSource)
import           Capability.State            (HasState)
import           Capability.Writer           (HasWriter)
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Cont          (MonadCont)
import           Control.Monad.Except        (MonadError)
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.IO.Unlift     (MonadUnliftIO)
import           Control.Monad.Primitive     (PrimMonad)
import           Control.Monad.RWS           (MonadRWS)
import           Control.Monad.Reader        (MonadReader)
import           Control.Monad.State         (MonadState)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Writer        (MonadWriter)
import           Data.Kind                   (Type)

-- * @unliftio@

avail [t| MonadIO |]
avail' [[t| MonadIO |]] [t| MonadUnliftIO |]

-- * @monad-control@

with1 $ \m -> avail [t| MonadBase $m |]
with1 $ \m -> avail' [[t| MonadBase $m |]] [t| MonadBaseControl $m |]

-- * @exceptions@

avail [t| MonadThrow |]
avail' [[t| MonadThrow |]] [t| MonadCatch |]
avail' [[t| MonadCatch |]] [t| MonadMask |]

-- * @primitive@

avail [t| PrimMonad |]

-- * @mtl@

avail [t| MonadCont |]
with1 $ \r -> avail [t| MonadReader $r |]
with1 $ \w -> avail [t| MonadWriter $w |]
with1 $ \s -> avail [t| MonadState $s |]
with1 $ \e -> avail [t| MonadError $e |]
with3 $ \r w s -> avail'
  [ [t| MonadReader $r |]
  , [t| MonadWriter $w |]
  , [t| MonadState $s |]
  ] [t| MonadRWS $r $w $s |]

-- * @capability@

with2 $ \tag a -> avail [t| HasSource $tag $a |]
with2 $ \tag a -> avail [t| HasSink $tag $a |]
with2 $ \tag e -> avail [t| HasThrow $tag $e |]

with2 $ \tag r -> avail' [[t| HasSource $tag $r |]] [t| HasReader $tag $r |]
with2 $ \tag w -> avail' [[t| HasSink $tag $w |]] [t| HasWriter $tag $w |]
with2 $ \tag e -> avail' [[t| HasThrow $tag $e |]] [t| HasCatch $tag $e |]

with2 $ \tag s -> avail'
  [ [t| HasSource $tag $s |]
  , [t| HasSink $tag $s |]
  ] [t| HasState $tag $s |]
