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
    DFeedback.DriverProfileResponse (..),
  )
where

import AWS.S3 (FileType (..))
import qualified Beckn.ACL.Rating as ACL
import qualified Data.Text as T
import qualified Domain.Action.UI.Feedback as DFeedback
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import qualified Environment as App
import EulerHS.Prelude hiding (product)
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.CallBPP as CallBPP
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import Tools.Auth

-------- Feedback Flow and Know your Driver Flow----------
type API =
  "feedback"
    :> ( "rateRide"
           :> TokenAuth
           :> ReqBody '[JSON] DFeedback.FeedbackReq
           :> Post '[JSON] APISuccess
       )
    :<|> "knowYourDriver"
      :> TokenAuth
      :> Capture "rideId" (Id DRide.Ride)
      :> Get '[JSON] DFeedback.DriverProfileResponse
    :<|> "knowYourFavDriver"
      :> TokenAuth
      :> Capture "driverId" Text
      :> Get '[JSON] DFeedback.DriverProfileResponse

handler :: App.FlowServer API
handler =
  rating
    :<|> knowYourDriver
    :<|> knowYourFavDriver

rating :: (Id Person.Person, Id Merchant.Merchant) -> DFeedback.FeedbackReq -> App.FlowHandler APISuccess
rating (personId, merchantId) request = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  let filePath = "/path/to/dummy/video.mp4"
      contentType = T.pack "video/mp4"
      fileType = Video
      feedbackMediaUploadReqVideo =
        DFeedback.FeedbackMediaUploadReq
          { file = filePath,
            reqContentType = contentType,
            fileType = fileType
          }
  _media <- DFeedback.audioFeedbackUpload (personId, merchantId) feedbackMediaUploadReqVideo
  dFeedbackRes <- DFeedback.feedback request personId
  becknReq <- ACL.buildRatingReqV2 dFeedbackRes
  fork "call bpp rating api" $ do
    isValueAddNP <- CQVAN.isValueAddNP dFeedbackRes.providerId
    when isValueAddNP . void . withLongRetry $ CallBPP.feedbackV2 dFeedbackRes.providerUrl becknReq merchantId
  pure Success

knowYourDriver :: (Id Person.Person, Id Merchant.Merchant) -> Id DRide.Ride -> App.FlowHandler DFeedback.DriverProfileResponse
knowYourDriver (personId, _merchantId) rideId = withFlowHandlerAPI . withPersonIdLogTag personId $ DFeedback.knowYourDriver rideId

knowYourFavDriver :: (Id Person.Person, Id Merchant.Merchant) -> Text -> App.FlowHandler DFeedback.DriverProfileResponse
knowYourFavDriver (personId, merchantId) driverId = withFlowHandlerAPI . withPersonIdLogTag personId $ DFeedback.knowYourFavDriver driverId merchantId

-- _audioFeedbackUpload :: (Id Person.Person, Id Merchant.Merchant) -> DFeedback.FeedbackMediaUploadReq ->  App.FlowHandler APISuccess
-- _audioFeedbackUpload (personId, merchantId) req = withFlowHandlerAPI $ DFeedback.audioFeedbackUpload (cast personId, cast merchantId) req
