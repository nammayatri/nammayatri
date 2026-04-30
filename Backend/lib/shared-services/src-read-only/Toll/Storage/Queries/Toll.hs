{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Toll.Storage.Queries.Toll where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Toll.Domain.Types.Toll
import qualified Toll.Storage.Beam.Toll as Beam
import qualified Toll.Storage.BeamFlow

create :: (Toll.Storage.BeamFlow.BeamFlow m r) => (Toll.Domain.Types.Toll.Toll -> m ())
create = createWithKV

createMany :: (Toll.Storage.BeamFlow.BeamFlow m r) => ([Toll.Domain.Types.Toll.Toll] -> m ())
createMany = traverse_ create

findAllTollsByMerchantOperatingCity :: (Toll.Storage.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Toll.Domain.Types.Toll.Toll]))
findAllTollsByMerchantOperatingCity merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

findByPrimaryKey :: (Toll.Storage.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Toll.Domain.Types.Toll.Toll -> m (Maybe Toll.Domain.Types.Toll.Toll))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Toll.Storage.BeamFlow.BeamFlow m r) => (Toll.Domain.Types.Toll.Toll -> m ())
updateByPrimaryKey (Toll.Domain.Types.Toll.Toll {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.isAutoRickshawAllowed isAutoRickshawAllowed,
      Se.Set Beam.isTwoWheelerAllowed isTwoWheelerAllowed,
      Se.Set Beam.name name,
      Se.Set Beam.currency (((Kernel.Prelude.Just . (.currency))) price),
      Se.Set Beam.price ((.amount) price),
      Se.Set Beam.tollEndGates tollEndGates,
      Se.Set Beam.tollStartGates tollStartGates,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Toll Toll.Domain.Types.Toll.Toll where
  fromTType' (Beam.TollT {..}) = do
    pure $
      Just
        Toll.Domain.Types.Toll.Toll
          { id = Kernel.Types.Id.Id id,
            isAutoRickshawAllowed = isAutoRickshawAllowed,
            isTwoWheelerAllowed = isTwoWheelerAllowed,
            name = name,
            price = Kernel.Types.Common.mkPrice currency price,
            tollEndGates = tollEndGates,
            tollStartGates = tollStartGates,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Toll Toll.Domain.Types.Toll.Toll where
  toTType' (Toll.Domain.Types.Toll.Toll {..}) = do
    Beam.TollT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.isAutoRickshawAllowed = isAutoRickshawAllowed,
        Beam.isTwoWheelerAllowed = isTwoWheelerAllowed,
        Beam.name = name,
        Beam.currency = ((Kernel.Prelude.Just . (.currency))) price,
        Beam.price = (.amount) price,
        Beam.tollEndGates = tollEndGates,
        Beam.tollStartGates = tollStartGates,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
