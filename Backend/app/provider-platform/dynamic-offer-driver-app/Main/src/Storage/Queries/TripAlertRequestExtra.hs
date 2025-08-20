{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TripAlertRequestExtra where

import Domain.Types.Alert
import Domain.Types.Alert.AlertRequestStatus
import qualified Domain.Types.AlertRequest
import qualified Domain.Types.FleetBadge as DFB
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.TripAlertRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TripAlertRequest as Beam
import Storage.Queries.OrphanInstances.TripAlertRequest

findTripAlertRequestsByFleetOwnerId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe AlertRequestType -> Maybe Text -> Maybe (Id DFB.FleetBadge) -> Maybe Text -> Maybe Int -> Maybe Int -> m [TripAlertRequest]
findTripAlertRequestsByFleetOwnerId merchantOpCityId fleetOwnerId mbFrom mbTo mbAlertRequestType mbDriverId mbFleetBadgeId mbRouteCode mbLimit mbOffset = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId.getId]
            <> [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId]
            <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq (fromJust mbFrom) | isJust mbFrom]
            <> [Se.Is Beam.createdAt $ Se.LessThanOrEq (fromJust mbTo) | isJust mbTo]
            <> [Se.Is Beam.alertRequestType $ Se.Eq (fromJust mbAlertRequestType) | isJust mbAlertRequestType]
            <> [Se.Is Beam.fleetBadgeId $ Se.Eq (mbFleetBadgeId <&> (.getId)) | isJust mbFleetBadgeId]
            <> [Se.Is Beam.driverId $ Se.Eq (fromJust mbDriverId) | isJust mbDriverId]
            <> [Se.Is Beam.routeCode $ Se.Eq (fromJust mbRouteCode) | isJust mbRouteCode]
        )
    ]
    (Se.Desc Beam.createdAt)
    (Just limitVal)
    (Just offsetVal)
  where
    limitVal = min (fromMaybe 10 mbLimit) 10
    offsetVal = fromMaybe 0 mbOffset

findLatestTripAlertRequest :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Text -> AlertRequestType -> Text -> Text -> m (Maybe TripAlertRequest)
findLatestTripAlertRequest merchantOpCityId fleetOwnerId alertRequestType driverId routeCode = do
  findTripAlertRequestsByFleetOwnerId merchantOpCityId fleetOwnerId Nothing Nothing (Just alertRequestType) (Just driverId) Nothing (Just routeCode) (Just 1) (Just 0) <&> listToMaybe

------------------------- multiple fleet owners -------------------------

findTripAlertRequestsByFleetOwnerIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> [Text] -> Maybe UTCTime -> Maybe UTCTime -> Maybe AlertRequestType -> Maybe Text -> Maybe [Id DFB.FleetBadge] -> Maybe Text -> Maybe AlertRequestStatus -> Maybe Int -> Maybe Int -> m [TripAlertRequest]
findTripAlertRequestsByFleetOwnerIds merchantOpCityId fleetOwnerIds mbFrom mbTo mbAlertRequestType mbDriverId mbFleetBadgeIds mbRouteCode mbalertStatus mbLimit mbOffset = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId.getId]
            <> [Se.Is Beam.fleetOwnerId $ Se.In fleetOwnerIds]
            <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq (fromJust mbFrom) | isJust mbFrom]
            <> [Se.Is Beam.createdAt $ Se.LessThanOrEq (fromJust mbTo) | isJust mbTo]
            <> [Se.Is Beam.alertRequestType $ Se.Eq (fromJust mbAlertRequestType) | isJust mbAlertRequestType]
            <> [Se.Is Beam.fleetBadgeId $ Se.In fleetBadgeIds | isJust mbFleetBadgeIds]
            <> [Se.Is Beam.driverId $ Se.Eq (fromJust mbDriverId) | isJust mbDriverId]
            <> [Se.Is Beam.routeCode $ Se.Eq (fromJust mbRouteCode) | isJust mbRouteCode]
            <> [Se.Is Beam.alertStatus $ Se.Eq mbalertStatus | isJust mbalertStatus]
        )
    ]
    (Se.Desc Beam.createdAt)
    (Just limitVal)
    (Just offsetVal)
  where
    fleetBadgeIds = maybe [] (map (pure . (.getId))) mbFleetBadgeIds
    limitVal = min (fromMaybe 10 mbLimit) 10
    offsetVal = fromMaybe 0 mbOffset

updateStatusWithReason ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.Alert.AlertRequestStatus.AlertRequestStatus -> Kernel.Types.Id.Id Domain.Types.AlertRequest.AlertRequest -> m ())
updateStatusWithReason alertStatus id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.alertStatus (Just alertStatus), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
