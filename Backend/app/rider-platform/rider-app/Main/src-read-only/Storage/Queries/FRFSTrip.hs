{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTrip where

import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSTrip
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTrip as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.FRFSTrip.FRFSTrip -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.FRFSTrip.FRFSTrip] -> m ()
createMany = traverse_ createWithKV

findAllByQuoteId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m ([Domain.Types.FRFSTrip.FRFSTrip])
findAllByQuoteId (Kernel.Types.Id.Id quoteId) = do
  findAllWithKV
    [ Se.Is Beam.quoteId $ Se.Eq quoteId
    ]

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FRFSTrip.FRFSTrip -> m (Maybe (Domain.Types.FRFSTrip.FRFSTrip))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FRFSTrip.FRFSTrip -> m (Maybe (Domain.Types.FRFSTrip.FRFSTrip))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.FRFSTrip.FRFSTrip -> m ()
updateByPrimaryKey Domain.Types.FRFSTrip.FRFSTrip {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppFulfillmentId $ bppFulfillmentId,
      Se.Set Beam.quoteId $ (Kernel.Types.Id.getId quoteId),
      Se.Set Beam.stationCode $ stationCode,
      Se.Set Beam.stationName $ stationName,
      Se.Set Beam.stationType $ stationType,
      Se.Set Beam.stopSequence $ stopSequence,
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId $ (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.FRFSTrip Domain.Types.FRFSTrip.FRFSTrip where
  fromTType' Beam.FRFSTripT {..} = do
    pure $
      Just
        Domain.Types.FRFSTrip.FRFSTrip
          { bppFulfillmentId = bppFulfillmentId,
            id = Kernel.Types.Id.Id id,
            quoteId = Kernel.Types.Id.Id quoteId,
            stationCode = stationCode,
            stationName = stationName,
            stationType = stationType,
            stopSequence = stopSequence,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSTrip Domain.Types.FRFSTrip.FRFSTrip where
  toTType' Domain.Types.FRFSTrip.FRFSTrip {..} = do
    Beam.FRFSTripT
      { Beam.bppFulfillmentId = bppFulfillmentId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.quoteId = Kernel.Types.Id.getId quoteId,
        Beam.stationCode = stationCode,
        Beam.stationName = stationName,
        Beam.stationType = stationType,
        Beam.stopSequence = stopSequence,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
