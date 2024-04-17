{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Booking (module Storage.Queries.Booking, module ReExport) where

import qualified Domain.Types.Booking
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Booking as Beam
import Storage.Queries.BookingExtra as ReExport
import Storage.Queries.Transformers.Booking

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m (Maybe Domain.Types.Booking.Booking))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]
