{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.TransporterConfig where

import qualified Domain.Types.Merchant as Domain
import qualified Domain.Types.TransporterConfig as Domain
import qualified Kernel.External.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (Meters)
import Kernel.Types.Id
import Kernel.Types.Time
import Storage.Tabular.Merchant (MerchantTId)

derivePersistField "Domain.ConfigKey"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    TransporterConfigT sql=transporter_config
      merchantId MerchantTId
      pickupLocThreshold Meters Maybe
      dropLocThreshold Meters Maybe
      rideTravelledDistThresholdWhenPickupOrDestIsDiff Meters Maybe
      rideTravelledDistThresholdWhenPickupAndDestIsSame Meters Maybe
      rideTimeEstimatedThreshold Seconds Maybe
      waitingTimeEstimatedThreshold Seconds Maybe
      maxRadius Meters
      minRadius Meters
      radiusStepSize Meters
      availabilityTimeWeightage Int
      acceptanceRatioWeightage Int
      cancellationRatioWeightage Int
      createdAt UTCTime
      updatedAt UTCTime
      fcmUrl Text
      fcmServiceAccount Text
      fcmTokenKeyPrefix Text
      Primary merchantId
      deriving Generic
    |]

instance TEntityKey TransporterConfigT where
  type DomainKey TransporterConfigT = Id Domain.Merchant
  fromKey (TransporterConfigTKey _id) = fromKey _id
  toKey id = TransporterConfigTKey $ toKey id

instance FromTType TransporterConfigT Domain.TransporterConfig where
  fromTType TransporterConfigT {..} = do
    fcmUrl' <- parseBaseUrl fcmUrl
    return $
      Domain.TransporterConfig
        { merchantId = fromKey merchantId,
          fcmConfig =
            FCM.FCMConfig
              { fcmUrl = fcmUrl',
                ..
              },
          ..
        }

instance ToTType TransporterConfigT Domain.TransporterConfig where
  toTType Domain.TransporterConfig {..} =
    TransporterConfigT
      { merchantId = toKey merchantId,
        fcmUrl = showBaseUrl fcmConfig.fcmUrl,
        fcmServiceAccount = fcmConfig.fcmServiceAccount,
        fcmTokenKeyPrefix = fcmConfig.fcmTokenKeyPrefix,
        ..
      }
