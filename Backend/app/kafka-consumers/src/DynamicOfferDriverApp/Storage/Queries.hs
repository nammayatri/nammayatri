 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module DynamicOfferDriverApp.Storage.Queries
  ( insertIntoDB,
    getAvailabilityInBucket,
    findLatestByDriverIdAndMerchantId,
    makeMapWithUniqueFields,
  )
where

import qualified Data.HashMap as HM
import qualified Data.Map as M
import DynamicOfferDriverApp.Storage.Tables
import qualified DynamicOfferDriverApp.Types as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (putMany)

makeMapWithUniqueFields :: [Domain.DriverAvailability] -> HM.Map ((Domain.MerchantId, Domain.DriverId), Text) Domain.DriverAvailability
makeMapWithUniqueFields = HM.fromList . map (\da -> (((da.merchantId, da.driverId), show (da.bucketStartTime, da.bucketEndTime) :: Text), da))

putMany ::
  HM.Map ((Domain.MerchantId, Domain.DriverId), Text) Domain.DriverAvailability ->
  HM.Map ((Domain.MerchantId, Domain.DriverId), Text) Domain.DriverAvailability ->
  SqlDB ()
putMany oldDriverAvaiabilityBuckets newDriverAvaiabilityBuckets = do
  let (toInsert', toDelete') =
        foldl'
          ( \(accToInsert, accToDelete) (toInsert, toDelete) ->
              (toInsert : accToInsert, maybe accToDelete (: accToDelete) toDelete)
          )
          ([], [])
          . HM.elems
          $ HM.mapWithKey (\key da -> (da, HM.lookup key oldDriverAvaiabilityBuckets)) newDriverAvaiabilityBuckets
  unless (null toDelete') (deleteBucketByDriverIdMerchantId toDelete')
  createMany toInsert'

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

insertIntoDB :: [Domain.DriverAvailability] -> [Domain.DriverAvailability] -> SqlDB ()
insertIntoDB oldValues newValues = putMany (makeMapWithUniqueFields oldValues) (makeMapWithUniqueFields newValues)

getAvailabilityInBucket :: (Esq.Transactionable m) => M.Map (Domain.MerchantId, Domain.DriverId) Domain.AvailabilityBucket -> m [Domain.DriverAvailability]
getAvailabilityInBucket driversAvailabilityBucket = buildDType $ do
  rows <- Esq.findAll' $ do
    row <- from $ table @DriverAvailabilityT
    where_ (foldr ((||.) . (\k -> k row)) (val False) fnArr)
    return row
  pure $ extractSolidType @Domain.DriverAvailability <$> rows
  where
    fnArr = concatMap (\(mIdAnddId, availabilityBuckets) -> map (constructQueryWithWheres mIdAnddId) (M.keys availabilityBuckets)) $ M.toList driversAvailabilityBucket

constructQueryWithWheres :: (Text, Text) -> (UTCTime, UTCTime) -> SqlExpr (Entity DriverAvailabilityT) -> SqlExpr (Value Bool)
constructQueryWithWheres (merchantId, driverId) (bucketStartTime, bucketEndTime) tbl =
  do
    tbl ^. DriverAvailabilityDriverId ==. val driverId
    &&. tbl ^. DriverAvailabilityMerchantId ==. val merchantId
    &&. tbl ^. DriverAvailabilityBucketStartTime ==. val bucketStartTime
    &&. tbl ^. DriverAvailabilityBucketEndTime ==. val bucketEndTime

deleteBucketByDriverIdMerchantId :: [Domain.DriverAvailability] -> SqlDB ()
deleteBucketByDriverIdMerchantId driversAvailabilityBucket =
  Esq.delete $ do
    row <- from $ table @DriverAvailabilityT
    where_ (foldr ((||.) . (\k -> k row)) (val False) fnArr)
  where
    fnArr = map (\da -> constructQueryWithWheres (da.merchantId, da.driverId) (da.bucketStartTime, da.bucketEndTime)) driversAvailabilityBucket
