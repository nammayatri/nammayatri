{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSQuote where

import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Station
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSQuote as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.FRFSQuote.FRFSQuote -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.FRFSQuote.FRFSQuote] -> m ()
createMany = traverse_ createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m (Maybe (Domain.Types.FRFSQuote.FRFSQuote))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findBySearchId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m (Maybe (Domain.Types.FRFSQuote.FRFSQuote))
findBySearchId (Kernel.Types.Id.Id searchId) = do
  findOneWithKV
    [ Se.Is Beam.searchId $ Se.Eq searchId
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m (Maybe (Domain.Types.FRFSQuote.FRFSQuote))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.FRFSQuote.FRFSQuote -> m ()
updateByPrimaryKey Domain.Types.FRFSQuote.FRFSQuote {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam._type $ _type,
      Se.Set Beam.bppItemId $ bppItemId,
      Se.Set Beam.bppSubscriberId $ bppSubscriberId,
      Se.Set Beam.from $ (Kernel.Types.Id.getId from),
      Se.Set Beam.price $ price,
      Se.Set Beam.providerDescription $ providerDescription,
      Se.Set Beam.providerId $ providerId,
      Se.Set Beam.providerName $ providerName,
      Se.Set Beam.quantity $ quantity,
      Se.Set Beam.searchId $ (Kernel.Types.Id.getId searchId),
      Se.Set Beam.to $ (Kernel.Types.Id.getId to),
      Se.Set Beam.validTill $ validTill,
      Se.Set Beam.vehicleType $ vehicleType,
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId $ (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.FRFSQuote Domain.Types.FRFSQuote.FRFSQuote where
  fromTType' Beam.FRFSQuoteT {..} = do
    pure $
      Just
        Domain.Types.FRFSQuote.FRFSQuote
          { _type = _type,
            bppItemId = bppItemId,
            bppSubscriberId = bppSubscriberId,
            from = Kernel.Types.Id.Id from,
            id = Kernel.Types.Id.Id id,
            price = price,
            providerDescription = providerDescription,
            providerId = providerId,
            providerName = providerName,
            quantity = quantity,
            searchId = Kernel.Types.Id.Id searchId,
            to = Kernel.Types.Id.Id to,
            validTill = validTill,
            vehicleType = vehicleType,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSQuote Domain.Types.FRFSQuote.FRFSQuote where
  toTType' Domain.Types.FRFSQuote.FRFSQuote {..} = do
    Beam.FRFSQuoteT
      { Beam._type = _type,
        Beam.bppItemId = bppItemId,
        Beam.bppSubscriberId = bppSubscriberId,
        Beam.from = Kernel.Types.Id.getId from,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.price = price,
        Beam.providerDescription = providerDescription,
        Beam.providerId = providerId,
        Beam.providerName = providerName,
        Beam.quantity = quantity,
        Beam.searchId = Kernel.Types.Id.getId searchId,
        Beam.to = Kernel.Types.Id.getId to,
        Beam.validTill = validTill,
        Beam.vehicleType = vehicleType,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
