{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BusinessHour where

import qualified Domain.Types.BusinessHour as Domain.Types.BusinessHour
import qualified Domain.Types.ServiceCategory as Domain.Types.ServiceCategory
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id as Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.BusinessHour as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.BusinessHour.BusinessHour -> m ()
create = createWithKV

instance FromTType' Beam.BusinessHour Domain.Types.BusinessHour.BusinessHour where
  fromTType' Beam.BusinessHourT {..} = do
    pure $
      Just
        Domain.Types.BusinessHour.BusinessHour
          { btype = btype,
            categoryId = Kernel.Types.Id.Id <$> categoryId,
            id = Kernel.Types.Id.Id id
          }

instance ToTType' Beam.BusinessHour Domain.Types.BusinessHour.BusinessHour where
  toTType' Domain.Types.BusinessHour.BusinessHour {..} = do
    Beam.BusinessHourT
      { Beam.btype = btype,
        Beam.categoryId = Kernel.Types.Id.getId <$> categoryId,
        Beam.id = Kernel.Types.Id.getId id
      }
