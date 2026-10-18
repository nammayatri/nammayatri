{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PersonRidePatternSummary where

import qualified Data.Aeson
import qualified Domain.Types.Person
import qualified Domain.Types.PersonRidePatternSummary
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PersonRidePatternSummary as Beam
import qualified Storage.Queries.Transformers.PersonRidePatternSummary

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PersonRidePatternSummary.PersonRidePatternSummary -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PersonRidePatternSummary.PersonRidePatternSummary] -> m ())
createMany = traverse_ create

findByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.PersonRidePatternSummary.PersonRidePatternSummary))
findByPersonId personId = do findOneWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateFreeFormSummary ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateFreeFormSummary freeFormSummary lastSummarizedAt personId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.freeFormSummary freeFormSummary, Se.Set Beam.lastSummarizedAt lastSummarizedAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.PersonRidePatternSummary.PersonRidePatternSummary))
findByPrimaryKey personId = do findOneWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PersonRidePatternSummary.PersonRidePatternSummary -> m ())
updateByPrimaryKey (Domain.Types.PersonRidePatternSummary.PersonRidePatternSummary {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.analysisWindowDays analysisWindowDays,
      Se.Set Beam.cancelledTaxiRides cancelledTaxiRides,
      Se.Set Beam.completedTaxiRides completedTaxiRides,
      Se.Set Beam.currency currency,
      Se.Set Beam.dayOfWeekCounts (Data.Aeson.toJSON dayOfWeekCounts),
      Se.Set Beam.freeFormSummary freeFormSummary,
      Se.Set Beam.hourOfDayCounts (Data.Aeson.toJSON hourOfDayCounts),
      Se.Set Beam.lastSummarizedAt lastSummarizedAt,
      Se.Set Beam.lastTripAt lastTripAt,
      Se.Set Beam.medianFareByService (Data.Aeson.toJSON medianFareByService),
      Se.Set Beam.medianTipValue (Data.Aeson.toJSON medianTipValue),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.publicTransportTrips publicTransportTrips,
      Se.Set Beam.routePatterns (Data.Aeson.toJSON routePatterns),
      Se.Set Beam.scheduledTaxiRides scheduledTaxiRides,
      Se.Set Beam.serviceTakenCounts (Data.Aeson.toJSON serviceTakenCounts),
      Se.Set Beam.tipComfortability tipComfortability,
      Se.Set Beam.totalTrips totalTrips,
      Se.Set Beam.uniqueRoutesTravelled uniqueRoutesTravelled,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.windowEnd windowEnd,
      Se.Set Beam.windowStart windowStart
    ]
    [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]

instance FromTType' Beam.PersonRidePatternSummary Domain.Types.PersonRidePatternSummary.PersonRidePatternSummary where
  fromTType' (Beam.PersonRidePatternSummaryT {..}) = do
    pure $
      Just
        Domain.Types.PersonRidePatternSummary.PersonRidePatternSummary
          { analysisWindowDays = analysisWindowDays,
            cancelledTaxiRides = cancelledTaxiRides,
            completedTaxiRides = completedTaxiRides,
            createdAt = createdAt,
            currency = currency,
            dayOfWeekCounts = Storage.Queries.Transformers.PersonRidePatternSummary.decodeOrEmpty dayOfWeekCounts,
            freeFormSummary = freeFormSummary,
            hourOfDayCounts = Storage.Queries.Transformers.PersonRidePatternSummary.decodeOrEmpty hourOfDayCounts,
            lastSummarizedAt = lastSummarizedAt,
            lastTripAt = lastTripAt,
            medianFareByService = Storage.Queries.Transformers.PersonRidePatternSummary.decodeOrEmpty medianFareByService,
            medianTipValue = Storage.Queries.Transformers.PersonRidePatternSummary.decodeOrEmpty medianTipValue,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            publicTransportTrips = publicTransportTrips,
            routePatterns = Storage.Queries.Transformers.PersonRidePatternSummary.decodeOrEmpty routePatterns,
            scheduledTaxiRides = scheduledTaxiRides,
            serviceTakenCounts = Storage.Queries.Transformers.PersonRidePatternSummary.decodeOrEmpty serviceTakenCounts,
            tipComfortability = tipComfortability,
            totalTrips = totalTrips,
            uniqueRoutesTravelled = uniqueRoutesTravelled,
            updatedAt = updatedAt,
            windowEnd = windowEnd,
            windowStart = windowStart
          }

instance ToTType' Beam.PersonRidePatternSummary Domain.Types.PersonRidePatternSummary.PersonRidePatternSummary where
  toTType' (Domain.Types.PersonRidePatternSummary.PersonRidePatternSummary {..}) = do
    Beam.PersonRidePatternSummaryT
      { Beam.analysisWindowDays = analysisWindowDays,
        Beam.cancelledTaxiRides = cancelledTaxiRides,
        Beam.completedTaxiRides = completedTaxiRides,
        Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.dayOfWeekCounts = Data.Aeson.toJSON dayOfWeekCounts,
        Beam.freeFormSummary = freeFormSummary,
        Beam.hourOfDayCounts = Data.Aeson.toJSON hourOfDayCounts,
        Beam.lastSummarizedAt = lastSummarizedAt,
        Beam.lastTripAt = lastTripAt,
        Beam.medianFareByService = Data.Aeson.toJSON medianFareByService,
        Beam.medianTipValue = Data.Aeson.toJSON medianTipValue,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.publicTransportTrips = publicTransportTrips,
        Beam.routePatterns = Data.Aeson.toJSON routePatterns,
        Beam.scheduledTaxiRides = scheduledTaxiRides,
        Beam.serviceTakenCounts = Data.Aeson.toJSON serviceTakenCounts,
        Beam.tipComfortability = tipComfortability,
        Beam.totalTrips = totalTrips,
        Beam.uniqueRoutesTravelled = uniqueRoutesTravelled,
        Beam.updatedAt = updatedAt,
        Beam.windowEnd = windowEnd,
        Beam.windowStart = windowStart
      }
