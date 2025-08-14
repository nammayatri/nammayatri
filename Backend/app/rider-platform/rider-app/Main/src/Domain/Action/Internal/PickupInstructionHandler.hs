{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.PickupInstructionHandler where

import qualified AWS.S3 as S3
import Data.Aeson (Result (Success), fromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Geohash as Geohash
import qualified Data.Text as T
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Ride as DRide
import Environment
import qualified IssueManagement.Storage.Queries.MediaFile as MFQuery
import Kernel.Prelude
import Kernel.Utils.Common
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.PickupInstructions as QPI
import Tools.Error

handlePickupInstruction :: DRide.Ride -> DBooking.Booking -> Value -> Flow ()
handlePickupInstruction ride booking driverIdValue = do
  logInfo $ "PickupInstructionHandler: Processing pickup instruction for ride: " <> ride.id.getId

  -- Extract driverId from Value type
  driverId <- extractDriverId driverIdValue
  logInfo $ "PickupInstructionHandler: Driver ID extracted: " <> driverId

  -- Get pickup location coordinates (fromLocation is already a Location object)
  let fromLocation = ride.fromLocation
  logInfo $ "PickupInstructionHandler: Pickup location found - lat: " <> show fromLocation.lat <> ", lon: " <> show fromLocation.lon

  -- Generate geohash from pickup coordinates
  geohash <-
    Geohash.encode 8 (fromLocation.lat, fromLocation.lon)
      & fromMaybeM (InvalidRequest "Invalid pickup location")
  logInfo $ "PickupInstructionHandler: Generated geohash: " <> T.pack geohash

  -- Query pickup instruction for this rider at this location
  mbPickupInstruction <- QPI.findByPersonIdAndGeohash booking.riderId (T.pack geohash)

  case mbPickupInstruction of
    Nothing -> do
      logInfo "PickupInstructionHandler: No pickup instruction found for this location, skipping"
      pure () -- No instruction found, skip silently
    Just pickupInstruction -> do
      logInfo $ "PickupInstructionHandler: Pickup instruction found - id: " <> pickupInstruction.id.getId

      -- Get audio base64 if mediaFileId exists
      mbAudioBase64 <- case pickupInstruction.mediaFileId of
        Just mediaFileId -> do
          logInfo $ "PickupInstructionHandler: Processing audio file - mediaFileId: " <> mediaFileId.getId
          mbMediaFile <- MFQuery.findById mediaFileId
          case mbMediaFile of
            Just mediaFile -> case mediaFile.s3FilePath of
              Just s3Path -> do
                logInfo $ "PickupInstructionHandler: Fetching audio from S3: " <> s3Path
                result <- try @_ @SomeException $ S3.get (T.unpack s3Path)
                case result of
                  Right audioContent -> do
                    logInfo "PickupInstructionHandler: Successfully retrieved audio content from S3"
                    return (Just audioContent)
                  Left err -> do
                    logError $ "PickupInstructionHandler: Failed to retrieve audio from S3: " <> show err
                    return Nothing
              Nothing -> do
                logInfo "PickupInstructionHandler: No S3 path found for media file"
                return Nothing
            Nothing -> do
              logInfo $ "PickupInstructionHandler: Media file not found for ID: " <> mediaFileId.getId
              return Nothing
        Nothing -> do
          logInfo "PickupInstructionHandler: No media file associated with this instruction"
          return Nothing

      -- Call driver app internal API
      merchant <- QM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
      logInfo $ "PickupInstructionHandler: Calling driver app API - driverId: " <> driverId

      result <-
        try @_ @SomeException $
          CallBPPInternal.sendPickupInstruction
            merchant.driverOfferApiKey
            merchant.driverOfferBaseUrl
            driverId
            (Just pickupInstruction.instruction)
            mbAudioBase64

      case result of
        Right _ -> logInfo "PickupInstructionHandler: Successfully sent pickup instruction to driver app"
        Left err -> logError $ "PickupInstructionHandler: Failed to send pickup instruction to driver app: " <> show err

extractDriverId :: Value -> Flow Text
extractDriverId driverIdValue = case fromJSON driverIdValue of
  Success driverId -> pure driverId
  Aeson.Error err -> throwError $ InvalidRequest ("Invalid driver ID format: " <> T.pack err)
