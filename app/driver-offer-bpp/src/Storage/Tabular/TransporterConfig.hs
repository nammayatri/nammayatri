{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.TransporterConfig where

import qualified Beckn.External.FCM.Types as FCM
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common (Meters)
import Beckn.Types.Id
import Beckn.Types.Time
import qualified Domain.Types.Merchant as Domain
import qualified Domain.Types.TransporterConfig as Domain
import Storage.Tabular.Merchant (MerchantTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    TransporterConfigT sql=transporter_config
      merchantId MerchantTId
      pickupLocThreshold Meters Maybe
      dropLocThreshold Meters Maybe
      rideTravelledDistanceThreshold Meters Maybe
      rideTimeEstimatedThreshold Seconds Maybe
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
