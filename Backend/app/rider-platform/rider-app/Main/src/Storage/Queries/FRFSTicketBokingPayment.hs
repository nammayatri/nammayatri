{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicketBokingPayment
  ( module Storage.Queries.FRFSTicketBokingPayment,
    module Reexport,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketBookingPayment as Beam
import Storage.Queries.FRFSTicketBookingPayment as Reexport

findNewTBPByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DFRFSTicketBooking.FRFSTicketBooking -> m (Maybe DFRFSTicketBookingPayment.FRFSTicketBookingPayment)
findNewTBPByBookingId (Id bookingId) =
  findAllWithOptionsKV [Se.Is Beam.frfsTicketBookingId $ Se.Eq bookingId] (Se.Desc Beam.createdAt) (Just 1) Nothing <&> listToMaybe
