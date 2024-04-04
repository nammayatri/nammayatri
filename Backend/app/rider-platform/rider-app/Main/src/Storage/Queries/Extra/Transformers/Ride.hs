{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Storage.Queries.Extra.Transformers.Ride where

import Domain.Types.Location
import qualified Domain.Types.LocationMapping as DLM
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM)
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Tools.Error

getToLocation :: KvDbFlow m r => Text -> Text -> Maybe Text -> Maybe Text -> m (Maybe Location)
getToLocation id bookingId merchantId merchantOperatingCityId = do
  mappings <- QLM.findByEntityId id
  mbToLocationMapping <-
    if null mappings
      then do
        void $ QBooking.findById (Id bookingId)
        mbToLocationMapping <- QLM.getLatestEndByEntityId bookingId
        mbToLocationRideMapping <- (\toLocMap -> SLM.buildDropLocationMapping toLocMap.locationId id DLM.RIDE (Id <$> merchantId) (Id <$> merchantOperatingCityId)) `mapM` mbToLocationMapping
        whenJust mbToLocationRideMapping QLM.create
        return mbToLocationRideMapping
      else QLM.getLatestEndByEntityId id
  maybe (pure Nothing) (QL.findById . (.locationId)) mbToLocationMapping

getFromLocation :: KvDbFlow m r => Text -> Text -> Maybe Text -> Maybe Text -> m Location
getFromLocation id bookingId merchantId merchantOperatingCityId = do
  mappings <- QLM.findByEntityId id
  fromLocationMapping <-
    if null mappings
      then do
        void $ QBooking.findById (Id bookingId)
        fromLocationMapping <- QLM.getLatestStartByEntityId bookingId >>= fromMaybeM (FromLocationMappingNotFound bookingId)
        fromLocationRideMapping <- SLM.buildPickUpLocationMapping fromLocationMapping.locationId id DLM.RIDE (Id <$> merchantId) (Id <$> merchantOperatingCityId)
        QLM.create fromLocationRideMapping
        return fromLocationRideMapping
      else QLM.getLatestStartByEntityId id >>= fromMaybeM (FromLocationMappingNotFound id)
  QL.findById fromLocationMapping.locationId >>= fromMaybeM (FromLocationNotFound fromLocationMapping.locationId.getId)
