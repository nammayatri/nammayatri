module Core.ACL.Cancel (buildCancelReq) where

import Beckn.Prelude
import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.Cancel.Req as Cancel
import qualified Beckn.Types.Core.Taxi.Cancel.Req as ReqCancel
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Types.RideBookingCancellationReason as SBCR
import ExternalAPI.Flow
import Utils.Common

buildCancelReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DCancel.CancelRes ->
  m (BecknReq Cancel.CancelMessage)
buildCancelReq res = do
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  messageId <- generateGUID
  context <- buildTaxiContext Context.CANCEL messageId Nothing bapIDs.cabs bapURIs.cabs (Just res.bppId) (Just res.bppUrl)
  pure $ BecknReq context $ mkCancelMessage res

mkCancelMessage :: DCancel.CancelRes -> Cancel.CancelMessage
mkCancelMessage res = ReqCancel.CancelMessage res.bppBookingId.getId $ castCancellatonSource res.cancellationSource
  where
    castCancellatonSource = \case
      SBCR.ByUser -> ReqCancel.ByUser
      SBCR.ByDriver -> ReqCancel.ByDriver
      SBCR.ByOrganization -> ReqCancel.ByOrganization
      SBCR.ByAllocator -> ReqCancel.ByAllocator
