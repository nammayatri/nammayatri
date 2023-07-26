{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.AppInstalls where

import qualified Domain.Types.AppInstalls as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Version
import Storage.Tabular.Merchant (MerchantTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    AppInstallsT sql=app_installs
      id Text
      merchantId MerchantTId
      deviceToken Text
      source Text
      createdAt UTCTime
      updatedAt UTCTime
      appVersion Text Maybe
      bundleVersion Text Maybe
      platform Text Maybe
      Primary id
      UniqueAppInstallsMerchantIdAndDeviceTokenAndSource merchantId deviceToken source
      deriving Generic
    |]

instance TEntityKey AppInstallsT where
  type DomainKey AppInstallsT = Id Domain.AppInstalls
  fromKey (AppInstallsTKey _id) = Id _id
  toKey (Id id) = AppInstallsTKey id

instance FromTType AppInstallsT Domain.AppInstalls where
  fromTType AppInstallsT {..} = do
    bundleVersion' <- forM bundleVersion readVersion
    appVersion' <- forM appVersion readVersion
    return $
      Domain.AppInstalls
        { id = Id id,
          merchantId = fromKey merchantId,
          bundleVersion = bundleVersion',
          appVersion = appVersion',
          ..
        }

instance ToTType AppInstallsT Domain.AppInstalls where
  toTType Domain.AppInstalls {..} =
    AppInstallsT
      { id = getId id,
        merchantId = toKey merchantId,
        appVersion = versionToText <$> appVersion,
        bundleVersion = versionToText <$> bundleVersion,
        ..
      }
