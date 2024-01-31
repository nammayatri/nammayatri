{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSRecon where

import qualified Domain.Types.FRFSRecon
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSRecon as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.FRFSRecon.FRFSRecon -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.FRFSRecon.FRFSRecon] -> m ()
createMany = traverse_ createWithKV

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FRFSRecon.FRFSRecon -> m (Maybe (Domain.Types.FRFSRecon.FRFSRecon))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.FRFSRecon.FRFSRecon -> m ()
updateByPrimaryKey Domain.Types.FRFSRecon.FRFSRecon {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.beneficiaryIFSC $ beneficiaryIFSC,
      Se.Set Beam.buyerFinderFee $ buyerFinderFee,
      Se.Set Beam.collectorIFSC $ collectorIFSC,
      Se.Set Beam.collectorSubscriberId $ collectorSubscriberId,
      Se.Set Beam.date $ date,
      Se.Set Beam.destinationStationCode $ destinationStationCode,
      Se.Set Beam.differenceAmount $ differenceAmount,
      Se.Set Beam.fare $ fare,
      Se.Set Beam.frfsTicketBookingId $ (Kernel.Types.Id.getId frfsTicketBookingId),
      Se.Set Beam.message $ message,
      Se.Set Beam.mobileNumber $ mobileNumber,
      Se.Set Beam.networkOrderId $ networkOrderId,
      Se.Set Beam.receiverSubscriberId $ receiverSubscriberId,
      Se.Set Beam.settlementAmount $ settlementAmount,
      Se.Set Beam.settlementDate $ settlementDate,
      Se.Set Beam.settlementReferenceNumber $ settlementReferenceNumber,
      Se.Set Beam.sourceStationCode $ sourceStationCode,
      Se.Set Beam.ticketNumber $ ticketNumber,
      Se.Set Beam.ticketQty $ ticketQty,
      Se.Set Beam.time $ time,
      Se.Set Beam.totalOrderValue $ totalOrderValue,
      Se.Set Beam.transactionRefNumber $ transactionRefNumber,
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId $ (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.FRFSRecon Domain.Types.FRFSRecon.FRFSRecon where
  fromTType' Beam.FRFSReconT {..} = do
    pure $
      Just
        Domain.Types.FRFSRecon.FRFSRecon
          { beneficiaryIFSC = beneficiaryIFSC,
            buyerFinderFee = buyerFinderFee,
            collectorIFSC = collectorIFSC,
            collectorSubscriberId = collectorSubscriberId,
            date = date,
            destinationStationCode = destinationStationCode,
            differenceAmount = differenceAmount,
            fare = fare,
            frfsTicketBookingId = Kernel.Types.Id.Id frfsTicketBookingId,
            id = Kernel.Types.Id.Id id,
            message = message,
            mobileNumber = mobileNumber,
            networkOrderId = networkOrderId,
            receiverSubscriberId = receiverSubscriberId,
            settlementAmount = settlementAmount,
            settlementDate = settlementDate,
            settlementReferenceNumber = settlementReferenceNumber,
            sourceStationCode = sourceStationCode,
            ticketNumber = ticketNumber,
            ticketQty = ticketQty,
            time = time,
            totalOrderValue = totalOrderValue,
            transactionRefNumber = transactionRefNumber,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSRecon Domain.Types.FRFSRecon.FRFSRecon where
  toTType' Domain.Types.FRFSRecon.FRFSRecon {..} = do
    Beam.FRFSReconT
      { Beam.beneficiaryIFSC = beneficiaryIFSC,
        Beam.buyerFinderFee = buyerFinderFee,
        Beam.collectorIFSC = collectorIFSC,
        Beam.collectorSubscriberId = collectorSubscriberId,
        Beam.date = date,
        Beam.destinationStationCode = destinationStationCode,
        Beam.differenceAmount = differenceAmount,
        Beam.fare = fare,
        Beam.frfsTicketBookingId = Kernel.Types.Id.getId frfsTicketBookingId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.message = message,
        Beam.mobileNumber = mobileNumber,
        Beam.networkOrderId = networkOrderId,
        Beam.receiverSubscriberId = receiverSubscriberId,
        Beam.settlementAmount = settlementAmount,
        Beam.settlementDate = settlementDate,
        Beam.settlementReferenceNumber = settlementReferenceNumber,
        Beam.sourceStationCode = sourceStationCode,
        Beam.ticketNumber = ticketNumber,
        Beam.ticketQty = ticketQty,
        Beam.time = time,
        Beam.totalOrderValue = totalOrderValue,
        Beam.transactionRefNumber = transactionRefNumber,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
