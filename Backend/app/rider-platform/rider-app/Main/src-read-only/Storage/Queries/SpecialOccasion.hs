{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SpecialOccasion where

import qualified Data.Time.Calendar as Data.Time.Calendar
import qualified Domain.Types.BusinessHour as Domain.Types.BusinessHour
import qualified Domain.Types.SpecialOccasion as Domain.Types.SpecialOccasion
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Prelude as Kernel.Prelude
import qualified Kernel.Types.Id as Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.SpecialOccasion as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.SpecialOccasion.SpecialOccasion -> m ()
create = createWithKV

instance FromTType' Beam.SpecialOccasion Domain.Types.SpecialOccasion.SpecialOccasion where
  fromTType' Beam.SpecialOccasionT {..} = do
    pure $
      Just
        Domain.Types.SpecialOccasion.SpecialOccasion
          { businessHours = Kernel.Types.Id.Id <$> businessHours,
            date = date,
            dayOfWeek = dayOfWeek,
            description = description,
            entityId = entityId,
            id = Kernel.Types.Id.Id id,
            specialDayType = specialDayType
          }

instance ToTType' Beam.SpecialOccasion Domain.Types.SpecialOccasion.SpecialOccasion where
  toTType' Domain.Types.SpecialOccasion.SpecialOccasion {..} = do
    Beam.SpecialOccasionT
      { Beam.businessHours = Kernel.Types.Id.getId <$> businessHours,
        Beam.date = date,
        Beam.dayOfWeek = dayOfWeek,
        Beam.description = description,
        Beam.entityId = entityId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.specialDayType = specialDayType
      }
