{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Types.Flow (FlowR, runFlowR, HasFlowEnv) where

import Beckn.Types.Field
import Beckn.Types.Logging
import Beckn.Utils.Logging
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R

type FlowR r = ReaderT r L.Flow

instance Log (FlowR r) where
  logOutput = logOutput'
  withLogTag = withLogTag'

runFlowR :: R.FlowRuntime -> r -> FlowR r a -> IO a
runFlowR flowRt r x = I.runFlow flowRt . runReaderT x $ r

-- | Require monad to be Flow-based and have specified fields in Reader env.
type HasFlowEnv m r fields =
  ( L.MonadFlow m,
    MonadReader r m,
    HasFields r fields
  )
