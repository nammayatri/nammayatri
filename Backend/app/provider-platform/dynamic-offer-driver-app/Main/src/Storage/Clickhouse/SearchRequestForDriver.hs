{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.SearchRequestForDriver where

import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.Common as DI
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequestForDriver as DSRD
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.ServiceTierType as DServiceTierType
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import qualified Kernel.Types.Common as Common
import Kernel.Types.Id

data SearchRequestForDriverT f = SearchRequestForDriverT
  { id :: C f (Id DSRD.SearchRequestForDriver),
    driverId :: C f (Id DP.Driver),
    fromLocGeohash :: C f (Maybe Text),
    tripEstimatedDistance :: C f (Maybe Common.Meters),
    tripEstimatedDuration :: C f (Maybe Common.Seconds),
    merchantOperatingCityId :: C f (Id DMOC.MerchantOperatingCity),
    mode :: C f (Maybe DI.DriverMode),
    response :: C f (Maybe DI.SearchRequestForDriverResponse),
    vehicleServiceTier :: C f DServiceTierType.ServiceTierType,
    vehicleCategory :: C f (Maybe DVC.VehicleCategory),
    requestId :: C f (Id DSR.SearchRequest),
    searchTryId :: C f (Id DST.SearchTry),
    createdAt :: C f CH.DateTime --,
  }
  deriving (Generic)

deriving instance Show SearchRequestForDriver

instance CH.ClickhouseValue Common.Seconds

instance Hashable DVC.VehicleCategory

searchRequestForDriverTTable :: SearchRequestForDriverT (FieldModification SearchRequestForDriverT)
searchRequestForDriverTTable =
  SearchRequestForDriverT
    { id = "id",
      driverId = "driver_id",
      fromLocGeohash = "from_loc_geohash",
      merchantOperatingCityId = "merchant_operating_city_id",
      tripEstimatedDistance = "trip_estimated_distance",
      tripEstimatedDuration = "trip_estimated_duration",
      mode = "mode",
      response = "response",
      vehicleServiceTier = "vehicle_service_tier",
      vehicleCategory = "vehicle_category",
      requestId = "search_request_id",
      searchTryId = "search_try_id",
      createdAt = "created_at"
    }

type SearchRequestForDriver = SearchRequestForDriverT Identity

$(TH.mkClickhouseInstances ''SearchRequestForDriverT 'SELECT_FINAL_MODIFIER)

calulateSupplyDemandByGeohashAndServiceTierAndDistanceBin ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  UTCTime ->
  UTCTime ->
  m [(Maybe Text, Int, Int, Maybe DVC.VehicleCategory, Text)]
calulateSupplyDemandByGeohashAndServiceTierAndDistanceBin from to = do
  CH.findAll $
    CH.select_
      ( \srfd -> do
          let supplyCount = CH.count_ (CH.distinct srfd.driverId)
          let demandCount = CH.count_ (CH.distinct srfd.searchTryId)
          let tripDistanceBin =
                CH.case_
                  ( (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 0) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 2000), CH.valColumn "0-2")
                      NE.:| [ (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 2000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 4000), CH.valColumn "2-4"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 4000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 6000), CH.valColumn "4-6"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 6000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 8000), CH.valColumn "6-8"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 8000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 10000), CH.valColumn "8-10"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 10000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 12000), CH.valColumn "10-12"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 12000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 14000), CH.valColumn "12-14"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 14000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 16000), CH.valColumn "14-16"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 16000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 18000), CH.valColumn "16-18"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 18000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 20000), CH.valColumn "18-20"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 20000), CH.valColumn "more_than_20")
                            ]
                  )
                  (CH.valColumn "unknown")

          CH.groupBy (srfd.fromLocGeohash, srfd.vehicleCategory, tripDistanceBin) $ \(fromLocGeohash, vehicleCategory, tripDistanceBin') -> do
            (fromLocGeohash, supplyCount, demandCount, vehicleCategory, tripDistanceBin')
      )
      $ CH.filter_
        ( \srfd ->
            srfd.createdAt >=. CH.DateTime from
              CH.&&. srfd.createdAt <=. CH.DateTime to
              CH.&&. srfd.mode CH.==. Just DI.ONLINE
              CH.&&. CH.isNotNull srfd.fromLocGeohash
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE searchRequestForDriverTTable)

calulateAcceptanceCountByGeohashAndServiceTierAndDistanceBin ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  UTCTime ->
  UTCTime ->
  m [(Maybe Text, Int, Maybe DVC.VehicleCategory, Text)]
calulateAcceptanceCountByGeohashAndServiceTierAndDistanceBin from to = do
  CH.findAll $
    CH.select_
      ( \srfd -> do
          let acceptanceCount = CH.count_ (CH.distinct srfd.searchTryId)
          let tripDistanceBin =
                CH.case_
                  ( (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 0) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 2000), CH.valColumn "0-2")
                      NE.:| [ (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 2000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 4000), CH.valColumn "2-4"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 4000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 6000), CH.valColumn "4-6"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 6000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 8000), CH.valColumn "6-8"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 8000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 10000), CH.valColumn "8-10"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 10000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 12000), CH.valColumn "10-12"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 12000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 14000), CH.valColumn "12-14"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 14000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 16000), CH.valColumn "14-16"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 16000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 18000), CH.valColumn "16-18"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 18000) CH.&&.. srfd.tripEstimatedDistance CH.<.. CH.valColumn (Just 20000), CH.valColumn "18-20"),
                              (srfd.tripEstimatedDistance CH.>=.. CH.valColumn (Just 20000), CH.valColumn "more_than_20")
                            ]
                  )
                  (CH.valColumn "unknown")

          CH.groupBy (srfd.fromLocGeohash, srfd.vehicleCategory, tripDistanceBin) $ \(fromLocGeohash, vehicleCategory, tripDistanceBin') -> do
            (fromLocGeohash, acceptanceCount, vehicleCategory, tripDistanceBin')
      )
      $ CH.filter_
        ( \srfd ->
            srfd.createdAt >=. CH.DateTime from
              CH.&&. srfd.createdAt <=. CH.DateTime to
              CH.&&. CH.isNotNull srfd.fromLocGeohash
              CH.&&. (srfd.response CH.==. Just DI.Accept)
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE searchRequestForDriverTTable)

calulateSupplyDemandByGeohashAndServiceTier ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  UTCTime ->
  UTCTime ->
  m [(Maybe Text, Int, Int, Maybe DVC.VehicleCategory)]
calulateSupplyDemandByGeohashAndServiceTier from to = do
  CH.findAll $
    CH.select_
      ( \srfd -> do
          let supplyCount = CH.count_ (CH.distinct srfd.driverId)
          let demandCount = CH.count_ (CH.distinct srfd.searchTryId)
          CH.groupBy (srfd.fromLocGeohash, srfd.vehicleCategory) $ \(fromLocGeohash, vehicleCategory) -> do
            (fromLocGeohash, supplyCount, demandCount, vehicleCategory)
      )
      $ CH.filter_
        ( \srfd ->
            srfd.createdAt >=. CH.DateTime from
              CH.&&. srfd.createdAt <=. CH.DateTime to
              CH.&&. srfd.mode CH.==. Just DI.ONLINE
              CH.&&. CH.isNotNull srfd.fromLocGeohash
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE searchRequestForDriverTTable)

calulateAcceptanceCountByGeohashAndServiceTier ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  UTCTime ->
  UTCTime ->
  m [(Maybe Text, Int, Maybe DVC.VehicleCategory)]
calulateAcceptanceCountByGeohashAndServiceTier from to = do
  CH.findAll $
    CH.select_
      ( \srfd -> do
          let acceptanceCount = CH.count_ (CH.distinct srfd.searchTryId)
          CH.groupBy (srfd.fromLocGeohash, srfd.vehicleCategory) $ \(fromLocGeohash, vehicleCategory) -> do
            (fromLocGeohash, acceptanceCount, vehicleCategory)
      )
      $ CH.filter_
        ( \srfd ->
            srfd.createdAt >=. CH.DateTime from
              CH.&&. srfd.createdAt <=. CH.DateTime to
              CH.&&. CH.isNotNull srfd.fromLocGeohash
              CH.&&. (srfd.response CH.==. Just DI.Accept)
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE searchRequestForDriverTTable)

calulateDemandByCityAndServiceTier ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  UTCTime ->
  UTCTime ->
  m [(Id DMOC.MerchantOperatingCity, Int, Maybe DVC.VehicleCategory)]
calulateDemandByCityAndServiceTier from to = do
  CH.findAll $
    CH.select_
      ( \srfd -> do
          let demandCount = CH.count_ (CH.distinct srfd.searchTryId)
          CH.groupBy (srfd.merchantOperatingCityId, srfd.vehicleCategory) $ \(merchantOperatingCityId, vehicleCategory) -> do
            (merchantOperatingCityId, demandCount, vehicleCategory)
      )
      $ CH.filter_
        ( \srfd ->
            srfd.createdAt >=. CH.DateTime from
              CH.&&. srfd.createdAt <=. CH.DateTime to
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE searchRequestForDriverTTable)

calulateAcceptanceCountByCityAndServiceTier ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  UTCTime ->
  UTCTime ->
  m [(Id DMOC.MerchantOperatingCity, Int, Maybe DVC.VehicleCategory)]
calulateAcceptanceCountByCityAndServiceTier from to = do
  CH.findAll $
    CH.select_
      ( \srfd -> do
          let acceptanceCount = CH.count_ (CH.distinct srfd.searchTryId)
          CH.groupBy (srfd.merchantOperatingCityId, srfd.vehicleCategory) $ \(merchantOperatingCityId, vehicleCategory) -> do
            (merchantOperatingCityId, acceptanceCount, vehicleCategory)
      )
      $ CH.filter_
        ( \srfd ->
            srfd.createdAt >=. CH.DateTime from
              CH.&&. srfd.createdAt <=. CH.DateTime to
              CH.&&. (srfd.response CH.==. Just DI.Accept)
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE searchRequestForDriverTTable)

findByDriverId ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DP.Person ->
  UTCTime ->
  UTCTime ->
  Int ->
  Int ->
  m [(CH.DateTime, Id DSRD.SearchRequestForDriver, Id DSR.SearchRequest, Maybe Common.Meters, Maybe Common.Seconds)]
findByDriverId driverId from to limit offset = do
  CH.findAll $
    CH.select_ (\srfd -> CH.notGrouped (srfd.createdAt, srfd.id, srfd.requestId, srfd.tripEstimatedDistance, srfd.tripEstimatedDuration)) $
      CH.limit_ limit $
        CH.offset_ offset $
          CH.selectModifierOverride CH.NO_SELECT_MODIFIER $
            CH.filter_
              ( \srfd ->
                  srfd.driverId CH.==. driverId
                    CH.&&. srfd.createdAt >=. CH.DateTime from
                    CH.&&. srfd.createdAt <=. CH.DateTime to
              )
              (CH.all_ @CH.APP_SERVICE_CLICKHOUSE searchRequestForDriverTTable)

findByDriverIdForInfo ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DP.Person ->
  UTCTime ->
  UTCTime ->
  m (Int, Int, Int, Int)
findByDriverIdForInfo driverId from to = do
  acceptanceCount <- findSreqCountByDriverId driverId from to (Just DI.Accept)
  rejectionCount <- findSreqCountByDriverId driverId from to (Just DI.Reject)
  pulledCount <- findSreqCountByDriverId driverId from to (Just DI.Pulled)
  totalCount <- findSreqCountByDriverId driverId from to Nothing
  pure (acceptanceCount, rejectionCount, pulledCount, totalCount)

findSreqCountByDriverId ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DP.Person ->
  UTCTime ->
  UTCTime ->
  Maybe DI.SearchRequestForDriverResponse ->
  m Int
findSreqCountByDriverId driverId from to status = do
  res <-
    CH.findAll $
      CH.select_ (\srfd -> CH.aggregate $ CH.count_ srfd.id) $
        CH.selectModifierOverride CH.NO_SELECT_MODIFIER $
          CH.filter_
            ( \srfd ->
                srfd.driverId CH.==. driverId
                  CH.&&. srfd.createdAt >=. CH.DateTime from
                  CH.&&. srfd.createdAt <=. CH.DateTime to
                  CH.&&. case status of
                    Just s -> srfd.response CH.==. Just s
                    Nothing -> CH.isNotNull srfd.response
            )
            (CH.all_ @CH.APP_SERVICE_CLICKHOUSE searchRequestForDriverTTable)
  pure $ fromMaybe 0 (listToMaybe res)

concatFun :: [(Maybe Text, Int, Int, Maybe DVC.VehicleCategory)] -> [(Maybe Text, Int, Maybe DVC.VehicleCategory)] -> [(Maybe Text, Int, Int, Int, Maybe DVC.VehicleCategory)]
concatFun [] _ = []
concatFun _ [] = []
concatFun xs ys =
  let acceptanceMap =
        HM.fromListWith
          (+)
          [ ((fromLocGeohash, vehicleCategory), acceptanceCount)
            | (fromLocGeohash, acceptanceCount, vehicleCategory) <- ys
          ]

      result = [(fromLocGeohash, HM.findWithDefault 0 (fromLocGeohash, vehicleCategory) acceptanceMap, supplyCount, demandCount, vehicleCategory) | (fromLocGeohash, supplyCount, demandCount, vehicleCategory) <- xs, HM.member (fromLocGeohash, vehicleCategory) acceptanceMap]
   in result

concatFun' ::
  [(Id DMOC.MerchantOperatingCity, Int, Maybe DVC.VehicleCategory)] ->
  [(Id DMOC.MerchantOperatingCity, Int, Maybe DVC.VehicleCategory)] ->
  [(Id DMOC.MerchantOperatingCity, Int, Int, Maybe DVC.VehicleCategory)]
concatFun' xs ys =
  let demandMap =
        HM.fromListWith
          (+)
          [ ((cityId, vehicleCategory), demandCount)
            | (cityId, demandCount, vehicleCategory) <- ys
          ]

      result =
        [ ( cityId,
            acceptanceCount,
            HM.findWithDefault 0 (cityId, vehicleCategory) demandMap,
            vehicleCategory
          )
          | (cityId, acceptanceCount, vehicleCategory) <- xs,
            HM.member (cityId, vehicleCategory) demandMap
        ]
   in result

concatFun'' :: [(Maybe Text, Int, Int, Maybe DVC.VehicleCategory, Text)] -> [(Maybe Text, Int, Maybe DVC.VehicleCategory, Text)] -> [(Maybe Text, Int, Int, Int, Maybe DVC.VehicleCategory, Text)]
concatFun'' [] _ = []
concatFun'' _ [] = []
concatFun'' xs ys =
  let acceptanceMap =
        HM.fromListWith
          (+)
          [ ((fromLocGeohash, vehicleCategory, distanceBin), acceptanceCount)
            | (fromLocGeohash, acceptanceCount, vehicleCategory, distanceBin) <- ys
          ]

      result = [(fromLocGeohash, HM.findWithDefault 0 (fromLocGeohash, vehicleCategory, distanceBin) acceptanceMap, supplyCount, demandCount, vehicleCategory, distanceBin) | (fromLocGeohash, supplyCount, demandCount, vehicleCategory, distanceBin) <- xs, HM.member (fromLocGeohash, vehicleCategory, distanceBin) acceptanceMap]
   in result
