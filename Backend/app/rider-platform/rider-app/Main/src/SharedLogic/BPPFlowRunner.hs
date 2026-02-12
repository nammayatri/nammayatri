{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.BPPFlowRunner
  ( withBPPFlow,
    hasBPPEnv,
    withDirectBPP,
  )
where

import qualified "dynamic-offer-driver-app" Environment as BPPEnv
import qualified EulerHS.Runtime as R
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Flow (FlowR, runFlowR)
import Kernel.Utils.Common (fromMaybeM)

-- | Execute a BPP Flow action within the BAP process.
-- Switches the FlowR reader context from BAP's AppEnv to BPP's AppEnv.
-- The BPP AppEnv contains its own DB connections, Redis, Kafka, etc.
withBPPFlow ::
  ( MonadIO m,
    MonadReader r m,
    HasField "bppEnv" r (Maybe BPPEnv.AppEnv)
  ) =>
  R.FlowRuntime ->
  BPPEnv.Flow a ->
  m a
withBPPFlow bppFlowRt bppAction = do
  mbBppEnv <- asks (.bppEnv)
  bppEnv <- fromMaybeM (InternalError "BPP environment not initialized for direct call") mbBppEnv
  liftIO $ runFlowR bppFlowRt bppEnv bppAction

-- | Check if BPP environment is available for direct calls
hasBPPEnv ::
  ( MonadReader r m,
    HasField "bppEnv" r (Maybe BPPEnv.AppEnv)
  ) =>
  m Bool
hasBPPEnv = isJust <$> asks (.bppEnv)

-- | Run a direct BPP action if BPP FlowRuntime is available (valueAddedNP co-located),
-- otherwise fall back to the HTTP-based Beckn action.
withDirectBPP ::
  ( MonadReader r m,
    HasField "bppFlowRt" r (Maybe R.FlowRuntime)
  ) =>
  (R.FlowRuntime -> m ()) ->
  m () ->
  m ()
withDirectBPP directAction fallbackAction = do
  mbBppFlowRt <- asks (.bppFlowRt)
  case mbBppFlowRt of
    Just bppFlowRt -> directAction bppFlowRt
    Nothing -> fallbackAction
