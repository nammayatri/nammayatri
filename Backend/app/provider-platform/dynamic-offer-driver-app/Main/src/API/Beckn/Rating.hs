{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Rating (API, handler) where

import qualified Beckn.ACL.Rating as ACL
import qualified Beckn.OnDemand.Transformer.OndcScheduledRide.Rating as OSRRating
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.Rating as Rating
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Domain.Action.Beckn.Rating as DRating
import qualified Domain.Types.Booking as DBooking
import Domain.Types.Merchant (Merchant)
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.Beam.Types (TxnIdKey (..))
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BapMetadata as CQBapMetaData
import qualified Storage.Queries.Booking as QRB
import qualified Tools.ActorInfo as ActorInfo
import TransactionLogs.PushLogs

type API =
  Capture "merchantId" (Id Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> Rating.RatingAPIV2

handler :: FlowServer API
handler = rating

rating ::
  Id Merchant ->
  SignatureAuthResult ->
  Rating.RatingReqV2 ->
  FlowHandler AckResponse
rating merchantId (SignatureAuthResult _ subscriber) reqV2 = withFlowHandlerBecknAPI subscriber.subscriber_id . ActorInfo.withRequestIdActorInfo $ do
  transactionId <- Utils.getTransactionId reqV2.ratingReqContext
  L.setOptionLocal TxnIdKey transactionId
  Utils.withTransactionIdLogTag transactionId $ do
    logTagInfo "ratingAPIV2" $ "Received rating API call:-" <> show reqV2
    (mbBooking, isOndcScheduledRideSupportEnabled) <- getBookingAndOndcScheduledRideFlag reqV2.ratingReqMessage
    -- Pilot merchants get a separate v2.1.0 parser (order_id, ref_type/ref_id, feedbacks); Layer 1's parser would reject spec-compliant
    -- payloads that omit ratings[0].id or carry a non-Int ratings[0].value.
    dRatingReq <-
      if isOndcScheduledRideSupportEnabled
        then OSRRating.buildRatingReq subscriber reqV2
        else ACL.buildRatingReqV2 subscriber reqV2

    Redis.whenWithLockRedis (ratingLockKey dRatingReq.bookingId.getId) 60 $ do
      ride <- DRating.validateRequest dRatingReq mbBooking
      fork "rating request processing" $
        Redis.whenWithLockRedis (ratingProcessingLockKey dRatingReq.bookingId.getId) 60 $
          DRating.handler merchantId dRatingReq ride
      fork "rating received pushing ondc logs" do
        void $ pushLogs "rating" (toJSON reqV2) merchantId.getId "MOBILITY"
    pure Ack

-- | Reads the pilot flag from the booking's own city, not the city the BAP puts in the context. The booking id is order_id (v2.1.0)
-- or ratings[0].id (our own BAP). A missing booking or config means "not a pilot" rather than an error, so non-pilot requests fail
-- (or succeed) exactly as they did before the flag existed. The booking is returned so validateRequest doesn't read it again.
getBookingAndOndcScheduledRideFlag :: Spec.RatingReqMessage -> Flow (Maybe DBooking.Booking, Bool)
getBookingAndOndcScheduledRideFlag message = do
  let mbBookingId = message.ratingReqMessageOrderId <|> (safeHead (fromMaybe [] message.ratingReqMessageRatings) >>= (.ratingId))
  mbBooking <- maybe (pure Nothing) (B.runInReplica . QRB.findById . Id) mbBookingId
  case mbBooking of
    Nothing -> pure (Nothing, False)
    Just booking -> do
      mbBapMetadata <- CQBapMetaData.findBySubscriberIdDomainAndCity (Id booking.bapId) Domain.MOBILITY booking.merchantOperatingCityId
      pure (Just booking, CQBapMetaData.isOndcScheduledRideSupportEnabled mbBapMetadata)

ratingLockKey :: Text -> Text
ratingLockKey id = "Driver:Rating:BookingId-" <> id

ratingProcessingLockKey :: Text -> Text
ratingProcessingLockKey id = "Driver:Rating:Processing:BookingId-" <> id
