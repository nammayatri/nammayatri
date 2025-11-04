{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantState where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantState
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantState as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantState.MerchantState -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantState.MerchantState] -> m ())
createMany = traverse_ create

findByMerchantIdAndState ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.IndianState -> m (Maybe Domain.Types.MerchantState.MerchantState))
findByMerchantIdAndState merchantId state = do findOneWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId), Se.Is Beam.state $ Se.Eq state]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.IndianState -> m (Maybe Domain.Types.MerchantState.MerchantState))
findByPrimaryKey merchantId state = do findOneWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId), Se.Is Beam.state $ Se.Eq state]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantState.MerchantState -> m ())
updateByPrimaryKey (Domain.Types.MerchantState.MerchantState {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.allowedDestinationStates allowedDestinationStates, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.state $ Se.Eq state
        ]
    ]

instance FromTType' Beam.MerchantState Domain.Types.MerchantState.MerchantState where
  fromTType' (Beam.MerchantStateT {..}) = do
    pure $
      Just
        Domain.Types.MerchantState.MerchantState
          { allowedDestinationStates = allowedDestinationStates,
            merchantId = Kernel.Types.Id.Id merchantId,
            state = state,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MerchantState Domain.Types.MerchantState.MerchantState where
  toTType' (Domain.Types.MerchantState.MerchantState {..}) = do
    Beam.MerchantStateT
      { Beam.allowedDestinationStates = allowedDestinationStates,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.state = state,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
