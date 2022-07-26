module Product.Confirm
  ( confirm,
    ConfirmAPI,
    DConfirm.ConfirmReq (..),
    DConfirm.ConfirmLocationReq (..),
    ConfirmRes (..),
    onConfirm,
  )
where

import App.Types
import Beckn.Prelude hiding (init)
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.Init as ACL
import qualified Core.ACL.OnConfirm as ACL
import qualified Domain.Action.Beckn.OnConfirm as DOnConfirm
import qualified Domain.Action.UI.Confirm as DCOnfirm
import qualified Domain.Action.UI.Confirm as DConfirm
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Quote as Quote
import qualified ExternalAPI.Flow as ExternalAPI
import Servant
import Utils.Auth
import Utils.Common

type ConfirmAPI =
  "rideSearch"
    :> TokenAuth
    :> "quotes"
    :> Capture "quoteId" (Id Quote.Quote)
    :> "confirm"
    :> ReqBody '[JSON] DConfirm.ConfirmReq
    :> Post '[JSON] ConfirmRes

newtype ConfirmRes = ConfirmRes
  { bookingId :: Id DRB.Booking
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

-- It is confirm UI EP, but we call init beckn EP inside it. confirm beckn EP will be called in on_init
confirm ::
  Id SP.Person ->
  Id Quote.Quote ->
  DConfirm.ConfirmReq ->
  FlowHandler ConfirmRes
confirm personId quoteId req =
  withFlowHandlerAPI . withPersonIdLogTag personId $ do
    dConfirmRes <- DCOnfirm.confirm personId quoteId req
    void . ExternalAPI.init dConfirmRes.providerUrl =<< ACL.buildInitReq dConfirmRes
    return $
      ConfirmRes
        { bookingId = dConfirmRes.bookingId
        }

onConfirm ::
  SignatureAuthResult ->
  OnConfirm.OnConfirmReq ->
  FlowHandler AckResponse
onConfirm _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnConfirmReq <- ACL.buildOnConfirmReq req
  whenJust mbDOnConfirmReq DOnConfirm.onConfirm
  pure Ack
