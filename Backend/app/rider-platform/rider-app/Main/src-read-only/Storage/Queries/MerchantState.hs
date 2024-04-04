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
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantState as Beam

create :: KvDbFlow m r => (Domain.Types.MerchantState.MerchantState -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.MerchantState.MerchantState] -> m ())
createMany = traverse_ create

findByMerchantIdAndState :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.IndianState -> m (Maybe Domain.Types.MerchantState.MerchantState))
findByMerchantIdAndState (Kernel.Types.Id.Id merchantId) state = do findOneWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq merchantId, Se.Is Beam.state $ Se.Eq state]]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.IndianState -> m (Maybe Domain.Types.MerchantState.MerchantState))
findByPrimaryKey (Kernel.Types.Id.Id merchantId) state = do findOneWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq merchantId, Se.Is Beam.state $ Se.Eq state]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.MerchantState.MerchantState -> m ())
updateByPrimaryKey (Domain.Types.MerchantState.MerchantState {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.allowedDestinationStates allowedDestinationStates,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId), Se.Is Beam.state $ Se.Eq state]]

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
