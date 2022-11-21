module Core.ACL.Cancel (buildCancelReq) where

import Beckn.Prelude
import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.Cancel.Req as Cancel
import Beckn.Utils.Common
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Types.BookingCancellationReason as SBCR
import Environment

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
mkCancelMessage res = Cancel.CancelMessage res.bppBookingId.getId $ castCancellatonSource res.cancellationSource
  where
    castCancellatonSource = \case
      SBCR.ByUser -> Cancel.ByUser
      SBCR.ByDriver -> Cancel.ByDriver
      SBCR.ByMerchant -> Cancel.ByMerchant
      SBCR.ByAllocator -> Cancel.ByAllocator
      SBCR.ByApplication -> Cancel.ByApplication
