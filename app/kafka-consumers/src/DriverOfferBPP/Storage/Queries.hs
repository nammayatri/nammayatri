module DriverOfferBPP.Storage.Queries where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import DriverOfferBPP.Storage.Tables
import qualified DriverOfferBPP.Types as Domain

create :: Domain.DriverAvailability -> Esq.SqlDB ()
create = Esq.create

findLatestByDriverIdAndMerchantId :: (Esq.Transactionable m) => Text -> Text -> m (Maybe Domain.DriverAvailability)
findLatestByDriverIdAndMerchantId driverId merchantId = buildDType $
  fmap (fmap $ extractSolidType @Domain.DriverAvailability) $
    Esq.findOne' $ do
      dDriverAvailability <-
        from $ table @DriverAvailabilityT
      where_ $
        dDriverAvailability ^. DriverAvailabilityDriverId ==. val driverId
          &&. dDriverAvailability ^. DriverAvailabilityMerchantId ==. val merchantId
      orderBy [desc $ dDriverAvailability ^. DriverAvailabilityLastAvailableTime]
      limit 1
      pure dDriverAvailability

findAvailableTimeInBucketByDriverIdAndMerchantId :: (Esq.Transactionable m) => Text -> Text -> UTCTime -> UTCTime -> m (Maybe Domain.DriverAvailability)
findAvailableTimeInBucketByDriverIdAndMerchantId driverId merchantId bucketStartTime bucketEndTime = buildDType $
  fmap (fmap $ extractSolidType @Domain.DriverAvailability) $
    Esq.findOne' $ do
      dDriverAvailability <-
        from $ table @DriverAvailabilityT
      where_ $
        dDriverAvailability ^. DriverAvailabilityDriverId ==. val driverId
          &&. dDriverAvailability ^. DriverAvailabilityMerchantId ==. val merchantId
          &&. dDriverAvailability ^. DriverAvailabilityBucketStartTime ==. val bucketStartTime
          &&. dDriverAvailability ^. DriverAvailabilityBucketEndTime ==. val bucketEndTime
      orderBy [desc $ dDriverAvailability ^. DriverAvailabilityUpdatedAt]
      limit 1
      pure dDriverAvailability

createOrUpdateDriverAvailability :: Domain.DriverAvailability -> SqlDB ()
createOrUpdateDriverAvailability d@Domain.DriverAvailability {..} = do
  mbOldBucketAvailableTime <- findAvailableTimeInBucketByDriverIdAndMerchantId driverId merchantId bucketStartTime bucketEndTime
  case mbOldBucketAvailableTime of
    Nothing -> DriverOfferBPP.Storage.Queries.create d
    Just lastVal ->
      Esq.update $ \tbl -> do
        set
          tbl
          [ DriverAvailabilityTotalAvailableTime =. val (lastVal.totalAvailableTime + totalAvailableTime),
            DriverAvailabilityUpdatedAt =. val updatedAt,
            DriverAvailabilityLastAvailableTime =. val (ifAGBTA lastVal.lastAvailableTime lastAvailableTime)
          ]
        where_ $
          tbl ^. DriverAvailabilityDriverId ==. val driverId
            &&. tbl ^. DriverAvailabilityBucketStartTime ==. val bucketStartTime
            &&. tbl ^. DriverAvailabilityBucketEndTime ==. val bucketEndTime
  where
    ifAGBTA a b = if a > b then a else b
