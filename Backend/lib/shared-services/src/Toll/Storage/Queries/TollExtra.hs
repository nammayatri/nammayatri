{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Toll.Storage.Queries.TollExtra
  ( deleteById,
  )
where

import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Toll.Domain.Types.Toll
import qualified Toll.Storage.Beam.Toll as Beam
import Toll.Storage.BeamFlow

deleteById :: (BeamFlow m r) => Id Toll.Domain.Types.Toll.Toll -> m ()
deleteById tollId = deleteWithKV [Se.Is Beam.id $ Se.Eq (getId tollId)]

instance FromTType' Beam.Toll Toll.Domain.Types.Toll.Toll where
  fromTType' (Beam.TollT {..}) = do
    pure $
      Just
        Toll.Domain.Types.Toll.Toll
          { id = Id id,
            isAutoRickshawAllowed = isAutoRickshawAllowed,
            isTwoWheelerAllowed = isTwoWheelerAllowed,
            isAutoRickshawTollChargeApplicable = isAutoRickshawTollChargeApplicable,
            isTwoWheelerTollChargeApplicable = isTwoWheelerTollChargeApplicable,
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
      { Beam.id = getId id,
        Beam.isAutoRickshawAllowed = isAutoRickshawAllowed,
        Beam.isTwoWheelerAllowed = isTwoWheelerAllowed,
        Beam.isAutoRickshawTollChargeApplicable = isAutoRickshawTollChargeApplicable,
        Beam.isTwoWheelerTollChargeApplicable = isTwoWheelerTollChargeApplicable,
        Beam.name = name,
        Beam.currency = (Just . (.currency)) price,
        Beam.price = (.amount) price,
        Beam.tollEndGates = tollEndGates,
        Beam.tollStartGates = tollStartGates,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
