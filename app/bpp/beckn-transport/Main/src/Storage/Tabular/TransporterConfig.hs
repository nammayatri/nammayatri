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

instance TType TransporterConfigT Domain.TransporterConfig where
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
  toTType Domain.TransporterConfig {..} =
    TransporterConfigT
      { merchantId = toKey merchantId,
        fcmUrl = showBaseUrl fcmConfig.fcmUrl,
        fcmServiceAccount = fcmConfig.fcmServiceAccount,
        fcmTokenKeyPrefix = fcmConfig.fcmTokenKeyPrefix,
        ..
      }
