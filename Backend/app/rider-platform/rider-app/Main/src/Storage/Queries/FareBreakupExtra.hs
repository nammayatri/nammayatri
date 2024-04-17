{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareBreakupExtra where

import Domain.Types.Booking
import Domain.Types.FareBreakup
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FareBreakup as BeamFB
import Storage.Queries.OrphanInstances.FareBreakup

findAllByBookingId :: KvDbFlow m r => Id Booking -> m [FareBreakup]
findAllByBookingId bookingId = findAllWithKVAndConditionalDB [Se.Is BeamFB.bookingId $ Se.Eq $ getId bookingId] Nothing

deleteAllByBookingId :: KvDbFlow m r => Id Booking -> m ()
deleteAllByBookingId bookingId = deleteWithKV [Se.Is BeamFB.bookingId $ Se.Eq $ getId bookingId]
