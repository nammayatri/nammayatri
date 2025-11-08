{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.GetPickupInstructions where

import qualified AWS.S3 as S3
import qualified Data.Geohash as Geohash
import qualified Data.Text as T
import Domain.Types.Booking ()
import Domain.Types.Merchant ()
import Domain.Types.PickupInstructions ()
import Domain.Types.Ride ()
import qualified Domain.Types.Ride
import Environment
import qualified IssueManagement.Storage.Queries.MediaFile as QMF
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.IssueManagement ()
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.PickupInstructions as QPickupInstructions
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data PickupInstructionResp = PickupInstructionResp
  { instruction :: Maybe Text,
    audioBase64 :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getPickupInstructions :: Id Domain.Types.Ride.BPPRide -> Maybe Text -> Flow PickupInstructionResp
getPickupInstructions bppRideId apiKey = do
  logInfo $ "GetPickupInstructions: Processing request for bppRideId: " <> bppRideId.getId

  -- Find the ride by BPP ride ID
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideNotFound bppRideId.getId)
  logInfo $ "GetPickupInstructions: Found ride with bookingId: " <> ride.bookingId.getId

  -- Find the booking to get personId
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  logInfo $ "GetPickupInstructions: Found booking for personId: " <> booking.riderId.getId

  -- Validate API key
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == apiKey) $
    throwError (AuthBlocked "Invalid BAP internal api key")

  logInfo $ "GetPickupInstructions: API key validated"

  -- Get pickup location from ride
  let fromLocation = ride.fromLocation
  logInfo $ "GetPickupInstructions: Processing pickup location: " <> show fromLocation.lat <> "," <> show fromLocation.lon

  -- Generate geohash from pickup coordinates
  geohash <-
    Geohash.encode 8 (fromLocation.lat, fromLocation.lon)
      & fromMaybeM (InvalidRequest "Invalid pickup location")
  logInfo $ "GetPickupInstructions: Generated geohash: " <> T.pack geohash

  -- Query pickup instructions table using personId and geohash
  mbPickupInstruction <- QPickupInstructions.findByPersonIdAndGeohash booking.riderId (T.pack geohash)

  case mbPickupInstruction of
    Nothing -> do
      logInfo $ "GetPickupInstructions: No pickup instruction found for personId: " <> booking.riderId.getId <> " and geohash: " <> T.pack geohash
      pure $ PickupInstructionResp Nothing Nothing
    Just pickupInstruction -> do
      logInfo $ "GetPickupInstructions: Found pickup instruction: " <> pickupInstruction.instruction

      -- Get audio file if mediaFileId exists
      mbAudioBase64 <- case pickupInstruction.mediaFileId of
        Nothing -> do
          logInfo "GetPickupInstructions: No media file associated with pickup instruction"
          pure Nothing
        Just mediaFileId -> do
          logInfo $ "GetPickupInstructions: Fetching audio file for mediaFileId: " <> mediaFileId.getId
          mbMediaFile <- QMF.findById mediaFileId
          case mbMediaFile of
            Nothing -> do
              logError $ "GetPickupInstructions: Media file not found for id: " <> mediaFileId.getId
              pure Nothing
            Just mediaFile -> case mediaFile.s3FilePath of
              Just s3Path -> do
                logInfo $ "GetPickupInstructions: Found media file, fetching from S3: " <> s3Path
                result <- withTryCatch "S3:get:getPickupInstructions" $ S3.get $ T.unpack s3Path
                case result of
                  Left err -> do
                    logError $ "GetPickupInstructions: Failed to fetch audio from S3: " <> show err
                    pure Nothing
                  Right audioContent -> do
                    logInfo "GetPickupInstructions: Successfully fetched audio from S3"
                    pure $ Just audioContent
              Nothing -> do
                logInfo "GetPickupInstructions: No S3 path found for media file"
                pure Nothing

      logInfo $ "GetPickupInstructions: Returning response with instruction and audio"
      pure $ PickupInstructionResp (Just pickupInstruction.instruction) mbAudioBase64
