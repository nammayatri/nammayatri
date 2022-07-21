module Product.Init where

import App.Types
import Beckn.Prelude
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.Init as ACL
import qualified Core.ACL.OnInit as TaxiACL
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Action.UI.Init as DInit
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Person as SP
import qualified ExternalAPI.Flow as ExternalAPI
import Servant
import Utils.Auth
import Utils.Common

type InitAPI =
  "rideInit"
    :> TokenAuth
    :> ReqBody '[JSON] DInit.InitReq
    :> Post '[JSON] InitRes

newtype InitRes = InitRes
  { bookingId :: Id DRB.Booking
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

init ::
  Id SP.Person ->
  DInit.InitReq ->
  FlowHandler InitRes
init personId req =
  withFlowHandlerAPI . withPersonIdLogTag personId $ do
    dInitRes <- DInit.init personId req
    void . ExternalAPI.init dInitRes.providerUrl =<< ACL.buildInitReq dInitRes
    return $
      InitRes
        { bookingId = dInitRes.bookingId
        }

onInit ::
  SignatureAuthResult ->
  OnInit.OnInitReq ->
  FlowHandler AckResponse
onInit _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnInitReq <- TaxiACL.buildOnInitReq req
  whenJust mbDOnInitReq DOnInit.onInit
  pure Ack
