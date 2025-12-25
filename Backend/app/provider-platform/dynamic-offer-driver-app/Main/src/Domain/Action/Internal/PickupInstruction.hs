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
    audioUrl :: Maybe Text,
    customerName :: Maybe Text
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

  case req.audioUrl of
    Just _ -> logInfo "PickupInstruction: Audio instruction URL provided"
    Nothing -> logInfo "PickupInstruction: No audio instruction URL provided"

  -- Create entity data for the FCM notification
  let entityData =
        Notifications.PickupInstructionEntityData
          { instruction = req.instruction,
            audioUrl = req.audioUrl,
            customerName = req.customerName
          }

  -- Send FCM notification with pickup instruction
  logInfo $ "PickupInstruction: Sending FCM notification to driver: " <> driverId
  Notifications.sendPickupInstructionNotification driver.merchantOperatingCityId driver entityData

  logInfo $ "PickupInstruction: Successfully sent pickup instruction notification to driver: " <> driverId
  pure Success
