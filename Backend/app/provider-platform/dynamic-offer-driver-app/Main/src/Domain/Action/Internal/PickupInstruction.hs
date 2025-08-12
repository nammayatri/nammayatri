{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.PickupInstruction where

import Environment
import Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Notifications as Notifications

data PickupInstructionReq = PickupInstructionReq
  { driverId :: Text,
    instruction :: Maybe Text,
    audioBase64 :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

sendPickupInstruction :: Text -> Maybe Text -> PickupInstructionReq -> Flow APISuccess
sendPickupInstruction driverId apiKey req = do
  logInfo $ "PickupInstruction: Received pickup instruction for driver: " <> driverId

  -- Validate driver exists
  driver <- QPerson.findById (Id driverId) >>= fromMaybeM (PersonNotFound driverId)
  logInfo $ "PickupInstruction: Driver found: " <> driver.firstName

  -- Validate API key
  merchant <- QM.findById driver.merchantId >>= fromMaybeM (MerchantNotFound driver.merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError (AuthBlocked "Invalid BPP internal api key")

  logInfo $ "PickupInstruction: API key validated for merchant: " <> merchant.name

  -- Log the instruction details
  case req.instruction of
    Just instruction -> logInfo $ "PickupInstruction: Text instruction - " <> instruction
    Nothing -> logInfo "PickupInstruction: No text instruction provided"

  case req.audioBase64 of
    Just _ -> logInfo "PickupInstruction: Audio instruction provided"
    Nothing -> logInfo "PickupInstruction: No audio instruction provided"

  -- Send overlay notification with pickup instruction
  let overlayReq =
        FCM.FCMOverlayReq
          { title = Just "Pickup Instructions",
            description = req.instruction,
            imageUrl = Nothing,
            secondaryActions = Nothing,
            actions = [],
            link = Nothing,
            method = Nothing,
            reqBody = toJSON req.audioBase64, -- Include audio data in reqBody
            endPoint = Nothing,
            delay = Nothing,
            contactSupportNumber = Nothing,
            toastMessage = Nothing,
            okButtonText = Nothing,
            cancelButtonText = Nothing,
            actions2 = [],
            secondaryActions2 = Nothing,
            socialMediaLinks = Nothing,
            showPushNotification = Nothing
          }

  logInfo $ "PickupInstruction: Sending overlay notification to driver: " <> driverId
  Notifications.sendOverlay driver.merchantOperatingCityId driver overlayReq

  logInfo $ "PickupInstruction: Successfully sent pickup instruction overlay to driver: " <> driverId
  pure Success
