{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Rating
  ( API,
    handler,
    DFeedback.FeedbackReq (..),
  )
where

import qualified Beckn.ACL.Rating as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import BecknV2.Utils
import qualified Domain.Action.UI.Feedback as DFeedback
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Environment as App
import EulerHS.Prelude hiding (product)
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.CallBPP as CallBPP
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BecknConfig as QBC
import Tools.Auth
import TransactionLogs.Interface
import TransactionLogs.Interface.Types

-------- Feedback Flow ----------
type API =
  "feedback"
    :> ( "rateRide"
           :> TokenAuth
           :> ReqBody '[JSON] DFeedback.FeedbackReq
           :> Post '[JSON] APISuccess
       )

handler :: App.FlowServer API
handler = rating

rating :: (Id Person.Person, Id Merchant.Merchant) -> DFeedback.FeedbackReq -> App.FlowHandler APISuccess
rating (personId, _) request = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  dFeedbackRes <- DFeedback.feedback request
  becknReq <- ACL.buildRatingReqV2 dFeedbackRes
  fork "sending rating, pushing ondc logs" do
    let transactionLog = TransactionLogReq "rating" $ ReqLog (toJSON becknReq.ratingReqContext) (maskSensitiveData $ toJSON becknReq.ratingReqMessage)
    becknConfig <- QBC.findByMerchantIdDomainAndVehicle dFeedbackRes.booking.merchantId "MOBILITY" (Utils.mapVariantToVehicle dFeedbackRes.booking.vehicleVariant) >>= fromMaybeM (InternalError "Beckn Config not found")
    void $ pushTxnLogs (ONDCCfg $ ONDCConfig {apiToken = becknConfig.logsToken, url = becknConfig.logsUrl}) transactionLog -- shrey00 : Maybe validate ONDC response?
  void $ withLongRetry $ CallBPP.feedbackV2 dFeedbackRes.providerUrl becknReq
  pure Success
