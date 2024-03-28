{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.AppInstalls where

import qualified Domain.Types.AppInstalls
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Storage.Beam.AppInstalls as Beam
import Storage.Queries.Transformers.AppInstalls

instance FromTType' Beam.AppInstalls Domain.Types.AppInstalls.AppInstalls where
  fromTType' (Beam.AppInstallsT {..}) = do
    appVersion' <- readAppVersion appVersion
    bundleVersion' <- readBundleVersion bundleVersion
    pure $
      Just
        Domain.Types.AppInstalls.AppInstalls
          { appVersion = appVersion',
            bundleVersion = bundleVersion',
            createdAt = createdAt,
            deviceToken = deviceToken,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            platform = platform,
            source = source,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AppInstalls Domain.Types.AppInstalls.AppInstalls where
  toTType' (Domain.Types.AppInstalls.AppInstalls {..}) = do
    Beam.AppInstallsT
      { Beam.appVersion = Kernel.Prelude.fmap Kernel.Utils.Version.versionToText appVersion,
        Beam.bundleVersion = Kernel.Prelude.fmap Kernel.Utils.Version.versionToText bundleVersion,
        Beam.createdAt = createdAt,
        Beam.deviceToken = deviceToken,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.platform = platform,
        Beam.source = source,
        Beam.updatedAt = updatedAt
      }
