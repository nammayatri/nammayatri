{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetRcAssociationDailyStatsExtra where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Time.Calendar as DTC
import qualified Domain.Types.FleetRcAssociationDailyStats as DFRADS
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EncFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetRcAssociationDailyStats as Beam
import qualified Storage.Beam.VehicleRegistrationCertificate as BeamVRC

findAggregatedFleetRcStatsByFleetOwnerIdAndDateRange ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  DTC.Day ->
  DTC.Day ->
  m [Common.FleetVehicleStatsItem]
findAggregatedFleetRcStatsByFleetOwnerIdAndDateRange fleetOwnerId mbVehicleNo mbLimit mbOffset fromDay toDay = do
  mbRcId <- case mbVehicleNo of
    Just vehicleNo -> do
      certNumberHash <- getDbHash vehicleNo
      mbRc <- findOneWithKV [Se.Is BeamVRC.certificateNumberHash $ Se.Eq certNumberHash]
      pure $ BeamVRC.id <$> mbRc
    Nothing -> pure Nothing

  -- Fetch stats rows
  res <-
    findAllWithOptionsKV
      [ Se.And $
          [ Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId,
            Se.Is Beam.merchantLocalDate $ Se.GreaterThanOrEq fromDay,
            Se.Is Beam.merchantLocalDate $ Se.LessThanOrEq toDay
          ]
            <> (maybe [] (\rcId -> [Se.Is Beam.rcId $ Se.Eq rcId.getId]) mbRcId)
      ]
      (Se.Desc Beam.merchantLocalDate)
      (Just limit)
      (Just offset)

  let rcIds = map (getId . Beam.rcId) res

  rcList <-
    findAllWithKV
      [Se.Is BeamVRC.id $ Se.In rcIds]

  let rcMap = M.fromList [(BeamVRC.id rc, rc) | rc <- rcList]

  let groupByRcId =
        L.groupBy (\r1 r2 -> Beam.rcId r1 == Beam.rcId r2)
          . L.sortOn Beam.rcId
          $ res

  -- Aggregation logic
  let aggregateStats rows =
        foldl'
          ( \acc row ->
              acc{Common.totalEarnings = acc.totalEarnings + Beam.totalEarnings row,
                  Common.rideDistance = acc.rideDistance + Beam.rideDistance row,
                  Common.rideDuration = acc.rideDuration + Beam.rideDuration row,
                  Common.completedRides = acc.completedRides + Beam.totalCompletedRides row
                 }
          )
          emptyStatsItem
          rows

      emptyStatsItem =
        Common.FleetVehicleStatsItem
          { Common.vehicleNo = Nothing,
            Common.vehicleModel = Nothing,
            Common.rcId = Nothing,
            Common.totalEarnings = 0,
            Common.completedRides = 0,
            Common.rideDistance = 0,
            Common.rideDuration = 0,
            Common.earningPerKm = 0.0
          }

      aggregatedStats =
        [ let agg = aggregateStats rows
              rcId = Beam.rcId (head rows)
              rcInfo = M.lookup rcId rcMap
              totalEarnings = agg.totalEarnings
              completedRides = agg.completedRides
              rideDistance = agg.rideDistance
              rideDuration = agg.rideDuration
              earningPerKm =
                if rideDistance == 0 then 0 else totalEarnings / rideDistance
           in agg{Common.vehicleNo = rcInfo >>= (Just . BeamVRC.unencryptedCertificateNumber),
                  Common.vehicleModel = rcInfo >>= (Just . BeamVRC.vehicleModel),
                  Common.rcId = Just rcId,
                  Common.earningPerKm = earningPerKm
                 }
          | rows <- groupByRcId
        ]

  pure aggregatedStats
  where
    limit = min (fromMaybe 10 mbLimit) 10
    offset = fromMaybe 0 mbOffset
