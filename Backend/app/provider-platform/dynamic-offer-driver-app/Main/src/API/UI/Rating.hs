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
  )
where

import qualified Domain.Action.UI.FeedbackForm as DFeedbackForm
import qualified Domain.Action.UI.Rating as Domain
import Domain.Types.FeedbackForm
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude
import Kernel.External.Types (Language)
import Kernel.Types.APISuccess
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "feedback"
    :> ( "form"
           :> TokenAuth
           :> MandatoryQueryParam "language" Language
           :> QueryParam "rating" Int
           :> Get '[JSON] FeedbackFormList
           :<|> "rateRide"
             :> TokenAuth
             :> ReqBody '[JSON] CallBAPInternal.FeedbackReq
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  getFeedbackForm
    :<|> rateRide

getFeedbackForm :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Language -> Maybe Int -> FlowHandler FeedbackFormList
getFeedbackForm (_, _, merchantOpCityId) language mbRating = withFlowHandlerAPI $ DFeedbackForm.feedbackForm merchantOpCityId language mbRating

rateRide :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> CallBAPInternal.FeedbackReq -> FlowHandler APISuccess
rateRide (_, _, _) req = withFlowHandlerAPI $ Domain.rating req
