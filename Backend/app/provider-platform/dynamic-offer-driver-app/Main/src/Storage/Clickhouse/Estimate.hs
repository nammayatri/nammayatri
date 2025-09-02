{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.Estimate where

import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.Estimate as DE
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.ServiceTierType as DServiceTierType
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import qualified Kernel.Types.Common as Common
import Kernel.Types.Id

data EstimateT f = EstimateT
  { id :: C f (Id DE.Estimate),
    fromLocGeohash :: C f (Maybe Text),
    estimatedDistance :: C f (Maybe Common.Meters),
    estimatedDuration :: C f (Maybe Common.Seconds),
    merchantOperatingCityId :: C f (Id DMOC.MerchantOperatingCity),
    congestionMultiplier :: C f (Maybe Double),
    vehicleServiceTier :: C f DServiceTierType.ServiceTierType,
    requestId :: C f (Id DSR.SearchRequest),
    createdAt :: C f CH.DateTime --,
  }
  deriving (Generic)

deriving instance Show Estimate

instance CH.ClickhouseValue Common.Seconds

estimateTTable :: EstimateT (FieldModification EstimateT)
estimateTTable =
  EstimateT
    { id = "id",
      fromLocGeohash = "from_loc_geohash",
      merchantOperatingCityId = "merchant_operating_city_id",
      estimatedDistance = "estimated_distance",
      estimatedDuration = "estimated_duration",
      congestionMultiplier = "congestion_multiplier",
      vehicleServiceTier = "vehicle_service_tier",
      requestId = "request_id",
      createdAt = "created_at"
    }

type Estimate = EstimateT Identity

$(TH.mkClickhouseInstances ''EstimateT 'SELECT_FINAL_MODIFIER)

calulateCongestionByGeohashAndDistanceBin ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  UTCTime ->
  UTCTime ->
  m [(Maybe Text, Maybe Double, Text)]
calulateCongestionByGeohashAndDistanceBin from to = do
  CH.findAll $
    CH.select_
      ( \estimate -> do
          let congestion = CH.avg_ (estimate.congestionMultiplier)
          let tripDistanceBin =
                CH.case_
                  ( (estimate.estimatedDistance CH.>=.. CH.valColumn (Just 0) CH.&&.. estimate.estimatedDistance CH.<.. CH.valColumn (Just 2000), CH.valColumn "0-2")
                      NE.:| [ (estimate.estimatedDistance CH.>=.. CH.valColumn (Just 2000) CH.&&.. estimate.estimatedDistance CH.<.. CH.valColumn (Just 4000), CH.valColumn "2-4"),
                              (estimate.estimatedDistance CH.>=.. CH.valColumn (Just 4000) CH.&&.. estimate.estimatedDistance CH.<.. CH.valColumn (Just 6000), CH.valColumn "4-6"),
                              (estimate.estimatedDistance CH.>=.. CH.valColumn (Just 6000) CH.&&.. estimate.estimatedDistance CH.<.. CH.valColumn (Just 8000), CH.valColumn "6-8"),
                              (estimate.estimatedDistance CH.>=.. CH.valColumn (Just 8000) CH.&&.. estimate.estimatedDistance CH.<.. CH.valColumn (Just 10000), CH.valColumn "8-10"),
                              (estimate.estimatedDistance CH.>=.. CH.valColumn (Just 10000) CH.&&.. estimate.estimatedDistance CH.<.. CH.valColumn (Just 12000), CH.valColumn "10-12"),
                              (estimate.estimatedDistance CH.>=.. CH.valColumn (Just 12000) CH.&&.. estimate.estimatedDistance CH.<.. CH.valColumn (Just 14000), CH.valColumn "12-14"),
                              (estimate.estimatedDistance CH.>=.. CH.valColumn (Just 14000) CH.&&.. estimate.estimatedDistance CH.<.. CH.valColumn (Just 16000), CH.valColumn "14-16"),
                              (estimate.estimatedDistance CH.>=.. CH.valColumn (Just 16000) CH.&&.. estimate.estimatedDistance CH.<.. CH.valColumn (Just 18000), CH.valColumn "16-18"),
                              (estimate.estimatedDistance CH.>=.. CH.valColumn (Just 18000) CH.&&.. estimate.estimatedDistance CH.<.. CH.valColumn (Just 20000), CH.valColumn "18-20"),
                              (estimate.estimatedDistance CH.>=.. CH.valColumn (Just 20000), CH.valColumn "more_than_20")
                            ]
                  )
                  (CH.valColumn "unknown")

          CH.groupBy (estimate.fromLocGeohash, tripDistanceBin) $ \(fromLocGeohash, tripDistanceBin') -> do
            (fromLocGeohash, congestion, tripDistanceBin')
      )
      $ CH.filter_
        ( \estimate ->
            estimate.createdAt >=. CH.DateTime from
              CH.&&. estimate.createdAt <=. CH.DateTime to
              CH.&&. estimate.vehicleServiceTier `in_` [DServiceTierType.TAXI]
              CH.&&. CH.isNotNull estimate.fromLocGeohash
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE estimateTTable)

calulateCongestionByGeohash ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  UTCTime ->
  UTCTime ->
  m [(Maybe Text, Maybe Double)]
calulateCongestionByGeohash from to = do
  CH.findAll $
    CH.select_
      ( \estimate -> do
          let congestion = CH.avg_ (estimate.congestionMultiplier)
          CH.groupBy (estimate.fromLocGeohash) $ \(fromLocGeohash) -> do
            (fromLocGeohash, congestion)
      )
      $ CH.filter_
        ( \estimate ->
            estimate.createdAt >=. CH.DateTime from
              CH.&&. estimate.createdAt <=. CH.DateTime to
              CH.&&. estimate.vehicleServiceTier `in_` [DServiceTierType.TAXI]
              CH.&&. CH.isNotNull estimate.fromLocGeohash
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE estimateTTable)

calulateCongestionByCity ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  UTCTime ->
  UTCTime ->
  m [(Id DMOC.MerchantOperatingCity, Maybe Double)]
calulateCongestionByCity from to = do
  CH.findAll $
    CH.select_
      ( \estimate -> do
          let congestion = CH.avg_ (estimate.congestionMultiplier)
          CH.groupBy (estimate.merchantOperatingCityId) $ \(merchantOperatingCityId) -> do
            (merchantOperatingCityId, congestion)
      )
      $ CH.filter_
        ( \estimate ->
            estimate.createdAt >=. CH.DateTime from
              CH.&&. estimate.createdAt <=. CH.DateTime to
              CH.&&. estimate.vehicleServiceTier `in_` [DServiceTierType.TAXI]
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE estimateTTable)
