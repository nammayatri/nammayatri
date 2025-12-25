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
    processRating,
    DFeedback.FeedbackReq (..),
    DFeedback.DriverProfileResponse (..),
  )
where

import qualified Beckn.ACL.Rating as ACL
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
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import Storage.Beam.IssueManagement ()
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import Tools.Auth
import Tools.Error

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
      :> QueryParam "isImages" Bool
      :> Get '[JSON] DFeedback.DriverProfileResponse
    :<|> "knowYourFavDriver"
      :> TokenAuth
      :> Capture "driverId" Text
      :> QueryParam "isImages" Bool
      :> Get '[JSON] DFeedback.DriverProfileResponse

handler :: App.FlowServer API
handler =
  rating
    :<|> knowYourDriver
    :<|> knowYourFavDriver

rating :: (Id Person.Person, Id Merchant.Merchant) -> DFeedback.FeedbackReq -> App.FlowHandler APISuccess
rating (personId, merchantId) request = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  processRating (personId, merchantId) request

processRating :: (Id Person.Person, Id Merchant.Merchant) -> DFeedback.FeedbackReq -> App.Flow APISuccess
processRating (personId, merchantId) request = do
  dFeedbackRes <- DFeedback.feedback request personId
  becknReq <- ACL.buildRatingReqV2 dFeedbackRes
  fork "call bpp feedback apis" $ do
    isValueAddNP <- CQVAN.isValueAddNP dFeedbackRes.providerId
    when isValueAddNP $ do
      void . withLongRetry $ CallBPP.feedbackV2 dFeedbackRes.providerUrl becknReq merchantId
      merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
      let badgeMetadataList = dFeedbackRes.badgeMetadata
      void . withLongRetry $ CallBPPInternal.feedbackForm merchant.driverOfferBaseUrl (mkFeedbackFormReq request badgeMetadataList dFeedbackRes)
  pure Success
  where
    mkFeedbackFormReq :: DFeedback.FeedbackReq -> Maybe [CallBPPInternal.BadgeMetadata] -> DFeedback.FeedbackRes -> CallBPPInternal.FeedbackFormReq
    mkFeedbackFormReq req badgeMetadataList feedbackRes =
      CallBPPInternal.FeedbackFormReq
        { rideId = feedbackRes.bppRideId.getId,
          rating = Just req.rating,
          feedbackDetails = req.feedbackDetails,
          badges = badgeMetadataList,
          feedback = Just []
        }

knowYourDriver :: (Id Person.Person, Id Merchant.Merchant) -> Id DRide.Ride -> Maybe Bool -> App.FlowHandler DFeedback.DriverProfileResponse
knowYourDriver (personId, _merchantId) rideId withImages = withFlowHandlerAPI . withPersonIdLogTag personId $ DFeedback.knowYourDriver rideId withImages

knowYourFavDriver :: (Id Person.Person, Id Merchant.Merchant) -> Text -> Maybe Bool -> App.FlowHandler DFeedback.DriverProfileResponse
knowYourFavDriver (personId, merchantId) driverId withImages = withFlowHandlerAPI . withPersonIdLogTag personId $ DFeedback.knowYourFavDriver driverId merchantId withImages
