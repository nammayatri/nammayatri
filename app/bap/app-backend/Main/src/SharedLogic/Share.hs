module SharedLogic.Share where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Estimate as DEstimate
import Tools.Error

checkIfEstimateCancelled ::
  ( MonadFlow m
  ) =>
  Id DEstimate.Estimate ->
  Maybe DEstimate.EstimateStatus ->
  m ()
checkIfEstimateCancelled id status = do
  when (status == Just DEstimate.CANCELLED || status == Just DEstimate.DRIVER_QUOTE_CANCELLED) $ throwError $ EstimateCancelled id.getId
