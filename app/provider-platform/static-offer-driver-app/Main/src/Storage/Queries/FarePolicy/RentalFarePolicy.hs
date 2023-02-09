{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FarePolicy.RentalFarePolicy
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.FarePolicy.RentalFarePolicy
import qualified Domain.Types.FarePolicy.RentalFarePolicy as Domain
import Domain.Types.Merchant
import Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (Hours, Kilometers)
import Kernel.Types.Id
import Storage.Tabular.FarePolicy.RentalFarePolicy

create ::
  Domain.RentalFarePolicy ->
  SqlDB ()
create = Esq.create

-- it's possible to find deleted fare policies only by their id.
-- other function return only not deleted fare policies
-- (RentalFarePolicyDeleted ==. val False)

findById :: Transactionable m => Id RentalFarePolicy -> m (Maybe RentalFarePolicy)
findById = Esq.findById

findAllByMerchantId ::
  Transactionable m =>
  Id Merchant ->
  m [RentalFarePolicy]
findAllByMerchantId merchantId = do
  Esq.findAll $ do
    rentalFarePolicy <- from $ table @RentalFarePolicyT
    where_ $
      rentalFarePolicy ^. RentalFarePolicyMerchantId ==. val (toKey merchantId)
        &&. rentalFarePolicy ^. RentalFarePolicyDeleted ==. val False
    return rentalFarePolicy

markAllAsDeleted ::
  Id Merchant ->
  SqlDB ()
markAllAsDeleted merchantId = Esq.update $ \rentalFp -> do
  set
    rentalFp
    [RentalFarePolicyDeleted =. val True]
  where_ $ rentalFp ^. RentalFarePolicyMerchantId ==. val (toKey merchantId)

findByOffer ::
  Transactionable m =>
  Id Merchant ->
  Vehicle.Variant ->
  Kilometers ->
  Hours ->
  m (Maybe RentalFarePolicy)
findByOffer merchantId vehicleVariant_ baseDistance baseDuration = do
  Esq.findOne $ do
    rentalFarePolicy <- from $ table @RentalFarePolicyT
    where_ $
      rentalFarePolicy ^. RentalFarePolicyMerchantId ==. val (toKey merchantId)
        &&. rentalFarePolicy ^. RentalFarePolicyVehicleVariant ==. val vehicleVariant_
        &&. rentalFarePolicy ^. RentalFarePolicyBaseDistance ==. val baseDistance.getKilometers
        &&. rentalFarePolicy ^. RentalFarePolicyBaseDuration ==. val baseDuration.getHours
    return rentalFarePolicy
