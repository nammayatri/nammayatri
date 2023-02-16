{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FarePolicy.RestrictedExtraFare where

import qualified Domain.Types.FarePolicy.RestrictedExtraFare as Domain
import Domain.Types.Merchant
import qualified Domain.Types.Vehicle.Variant as Vehicle
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.FarePolicy.RestrictedExtraFare

create :: Domain.RestrictedExtraFare -> SqlDB ()
create = Esq.create

findMaxExtraFareByMerchantAndVehicle :: (Transactionable m) => Id Merchant -> Vehicle.Variant -> m [Domain.RestrictedExtraFare]
findMaxExtraFareByMerchantAndVehicle merchantId vehicleVariant = do
  findAll $ do
    restrictedExtraFare <- Esq.from $ table @RestrictedExtraFareT
    where_ $
      restrictedExtraFare ^. RestrictedExtraFareMerchantId ==. val (toKey merchantId)
        &&. restrictedExtraFare ^. RestrictedExtraFareVehicleVariant ==. val vehicleVariant
    orderBy [desc (restrictedExtraFare ^. RestrictedExtraFareMinTripDistance)]
    return restrictedExtraFare

findMaxExtraFareByMerchant :: (Transactionable m) => Id Merchant -> m [Domain.RestrictedExtraFare]
findMaxExtraFareByMerchant merchantId = do
  findAll $ do
    restrictedExtraFare <- Esq.from $ table @RestrictedExtraFareT
    where_ $
      restrictedExtraFare ^. RestrictedExtraFareMerchantId ==. val (toKey merchantId)
    orderBy [desc (restrictedExtraFare ^. RestrictedExtraFareMinTripDistance)]
    return restrictedExtraFare
