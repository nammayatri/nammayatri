module SharedLogic.Estimate where

import qualified Domain.Types.Estimate as DEstimate
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error

checkIfEstimateCancelled ::
  ( MonadFlow m
  ) =>
  Id DEstimate.Estimate ->
  Maybe DEstimate.EstimateStatus ->
  m ()
checkIfEstimateCancelled id status = do
  when (status == Just DEstimate.CANCELLED || status == Just DEstimate.DRIVER_QUOTE_CANCELLED) $ throwError $ EstimateCancelled id.getId
