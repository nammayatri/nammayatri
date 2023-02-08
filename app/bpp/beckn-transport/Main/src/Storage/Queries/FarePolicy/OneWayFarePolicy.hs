module Storage.Queries.FarePolicy.OneWayFarePolicy
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.FarePolicy.OneWayFarePolicy
import Domain.Types.Merchant
import Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FarePolicy.OneWayFarePolicy.PerExtraKmRate as QExtraKmRate
import Storage.Queries.FullEntityBuilders (buildFullOneWayFarePolicy)
import Storage.Tabular.FarePolicy.OneWayFarePolicy

findByMerchantIdAndVariant ::
  Transactionable m =>
  Id Merchant ->
  Vehicle.Variant ->
  m (Maybe OneWayFarePolicy)
findByMerchantIdAndVariant merchantId vehicleVariant_ =
  Esq.buildDType $ do
    mbFarePolicy <- Esq.findOne' $ do
      farePolicy <- from $ table @OneWayFarePolicyT
      where_ $
        farePolicy ^. OneWayFarePolicyMerchantId ==. val (toKey merchantId)
          &&. farePolicy ^. OneWayFarePolicyVehicleVariant ==. val vehicleVariant_
      return farePolicy
    mapM buildFullOneWayFarePolicy mbFarePolicy

findAllByMerchantId :: Transactionable m => Id Merchant -> m [OneWayFarePolicy]
findAllByMerchantId merchantId =
  Esq.buildDType $ do
    farePolicy <- Esq.findAll' $ do
      farePolicy <- from $ table @OneWayFarePolicyT
      where_ $ farePolicy ^. OneWayFarePolicyMerchantId ==. val (toKey merchantId)
      orderBy [asc $ farePolicy ^. OneWayFarePolicyVehicleVariant]
      return farePolicy
    mapM buildFullOneWayFarePolicy farePolicy

findById :: Transactionable m => Id OneWayFarePolicy -> m (Maybe OneWayFarePolicy)
findById fpId =
  Esq.buildDType $ do
    mbfarePolicy <- Esq.findById' fpId
    mapM buildFullOneWayFarePolicy mbfarePolicy

update :: OneWayFarePolicy -> SqlDB ()
update farePolicy = do
  now <- getCurrentTime
  withFullEntity farePolicy $ \(farePolicyT, perExtraKmRateList, _) -> do
    upsert'
      farePolicyT
      [ OneWayFarePolicyBaseFare =. val (fromIntegral <$> farePolicy.baseFare),
        OneWayFarePolicyNightShiftStart =. val (farePolicy.nightShiftStart),
        OneWayFarePolicyNightShiftEnd =. val (farePolicy.nightShiftEnd),
        OneWayFarePolicyNightShiftRate =. val farePolicy.nightShiftRate,
        OneWayFarePolicyUpdatedAt =. val now
      ]

    QExtraKmRate.deleteAll' farePolicy.merchantId farePolicy.vehicleVariant
    Esq.createMany' perExtraKmRateList
