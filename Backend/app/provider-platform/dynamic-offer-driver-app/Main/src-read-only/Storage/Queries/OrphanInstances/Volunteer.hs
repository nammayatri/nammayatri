{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Volunteer where

import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Volunteer
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, KvDbFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Volunteer as Beam

instance FromTType' Beam.Volunteer Domain.Types.Volunteer.Volunteer where
  fromTType' Beam.VolunteerT {..} = do
    pure $
      Just
        Domain.Types.Volunteer.Volunteer
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            place = place,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.Volunteer Domain.Types.Volunteer.Volunteer where
  toTType' Domain.Types.Volunteer.Volunteer {..} = do
    Beam.VolunteerT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.place = place,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
