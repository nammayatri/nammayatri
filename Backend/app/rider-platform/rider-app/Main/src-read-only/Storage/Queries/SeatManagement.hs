{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SeatManagement where

import qualified Data.Time.Calendar as Data.Time.Calendar
import qualified Domain.Types.SeatManagement as Domain.Types.SeatManagement
import qualified Domain.Types.ServiceCategory as Domain.Types.ServiceCategory
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Prelude as Kernel.Prelude
import qualified Kernel.Types.Id as Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.SeatManagement as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.SeatManagement.SeatManagement -> m ()
create = createWithKV

instance FromTType' Beam.SeatManagement Domain.Types.SeatManagement.SeatManagement where
  fromTType' Beam.SeatManagementT {..} = do
    pure $
      Just
        Domain.Types.SeatManagement.SeatManagement
          { blocked = blocked,
            booked = booked,
            date = date,
            id = Kernel.Types.Id.Id id,
            ticketServiceCategoryId = Kernel.Types.Id.Id ticketServiceCategoryId
          }

instance ToTType' Beam.SeatManagement Domain.Types.SeatManagement.SeatManagement where
  toTType' Domain.Types.SeatManagement.SeatManagement {..} = do
    Beam.SeatManagementT
      { Beam.blocked = blocked,
        Beam.booked = booked,
        Beam.date = date,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.ticketServiceCategoryId = Kernel.Types.Id.getId ticketServiceCategoryId
      }
