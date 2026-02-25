module Lib.JourneyLeg.Interface where

import API.Types.UI.FRFSTicketService
import qualified BecknV2.FRFS.Enums as Spec
import Control.Applicative ((<|>))
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Trip as DTrip
import qualified Kernel.External.MultiModal.Interface as EMInterface
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.JourneyLeg.Bus ()
import Lib.JourneyLeg.Metro ()
import Lib.JourneyLeg.Subway ()
import Lib.JourneyLeg.Taxi ()
import Lib.JourneyLeg.Types.Bus
import Lib.JourneyLeg.Types.Metro
import Lib.JourneyLeg.Types.Subway
import Lib.JourneyLeg.Types.Taxi
import Lib.JourneyLeg.Types.Walk
import Lib.JourneyLeg.Walk ()
import qualified Lib.JourneyModule.Types as JL
import qualified Lib.JourneyModule.Utils as JMU
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

getFare ::
  JL.GetFareFlow m r =>
  Maybe UTCTime ->
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe JMU.VehicleLiveRouteInfo ->
  EMInterface.MultiModalLeg ->
  DTrip.MultimodalTravelMode ->
  Maybe Text ->
  [Spec.ServiceTierType] ->
  [DFRFSQuote.FRFSQuoteType] ->
  Bool ->
  Maybe Spec.ServiceTierType ->
  m (Bool, Maybe JL.GetFareResponse)
getFare fromArrivalTime riderId merchantId merchantOperatingCityId mbRouteLiveInfo leg mode searchReqId blacklistedServiceTiers blacklistedFareQuoteTypes isSingleMode mbPreferredServiceTier = case mode of
  DTrip.Taxi -> do
    getFareReq :: TaxiLegRequest <- mkTaxiGetFareReq
    JL.getFare getFareReq
  DTrip.Bus -> do
    getFareReq :: Maybe BusLegRequest <- mkBusGetFareReq
    maybe (return (True, Nothing)) JL.getFare getFareReq
  DTrip.Metro -> do
    getFareReq :: Maybe MetroLegRequest <- mkMetroGetFareReq
    maybe (return (True, Nothing)) JL.getFare getFareReq
  DTrip.Subway -> do
    getFareReq :: Maybe SubwayLegRequest <- mkSubwayGetFareReq
    maybe (return (True, Nothing)) JL.getFare getFareReq
  DTrip.Walk -> do
    getFareReq :: WalkLegRequest <- mkWalkGetFareReq
    JL.getFare getFareReq
  where
    mkTaxiGetFareReq :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, JL.JourneyLeg TaxiLegRequest m) => m TaxiLegRequest
    mkTaxiGetFareReq = do
      merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
      merchantOpCity <- CQMOC.findById merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
      return $
        TaxiLegRequestGetFare $
          TaxiLegRequestGetFareData
            { startLocation = leg.startLocation.latLng,
              endLocation = leg.endLocation.latLng,
              distance = leg.distance,
              duration = leg.duration,
              merchant,
              merchantOpCity
            }

    mkBusGetFareReq :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, JL.JourneyLeg BusLegRequest m) => m (Maybe BusLegRequest)
    mkBusGetFareReq = do
      merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
      merchantOpCity <- CQMOC.findById merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
      let routeDetails = catMaybes $ map (mkRouteDetails (mbRouteLiveInfo <&> (.serviceType))) leg.routeDetails
          userPreferredServiceTier = mbPreferredServiceTier
      if length routeDetails /= length leg.routeDetails
        then do
          logError "Unable to Map Route Details for all Bus Route Sub Legs"
          return Nothing
        else
          return $
            Just $
              BusLegRequestGetFare $
                BusLegRequestGetFareData
                  { startLocation = leg.startLocation.latLng,
                    endLocation = leg.endLocation.latLng,
                    agencyGtfsId = leg.agency >>= (.gtfsId),
                    serviceType = mbRouteLiveInfo <&> (.serviceType),
                    ..
                  }

    mkMetroGetFareReq :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, JL.JourneyLeg MetroLegRequest m) => m (Maybe MetroLegRequest)
    mkMetroGetFareReq = do
      merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
      merchantOpCity <- CQMOC.findById merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
      let routeDetails = catMaybes $ map (mkRouteDetails Nothing) leg.routeDetails
      if length routeDetails /= length leg.routeDetails
        then do
          logError "Unable to Map Route Details for all Metro Route Sub Legs"
          return Nothing
        else
          return $
            Just $
              MetroLegRequestGetFare $
                MetroLegRequestGetFareData
                  { startLocation = leg.startLocation.latLng,
                    endLocation = leg.endLocation.latLng,
                    agencyGtfsId = leg.agency >>= (.gtfsId),
                    ..
                  }

    mkSubwayGetFareReq :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, JL.JourneyLeg SubwayLegRequest m) => m (Maybe SubwayLegRequest)
    mkSubwayGetFareReq = do
      merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
      merchantOpCity <- CQMOC.findById merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
      let routeDetails = catMaybes $ map (mkRouteDetails Nothing) leg.routeDetails
      if length routeDetails /= length leg.routeDetails
        then do
          logError "Unable to Map Route Details for all Subway Route Sub Legs"
          return Nothing
        else
          return $
            Just $
              SubwayLegRequestGetFare $
                SubwayLegRequestGetFareData
                  { startLocation = leg.startLocation.latLng,
                    endLocation = leg.endLocation.latLng,
                    agencyGtfsId = leg.agency >>= (.gtfsId),
                    searchReqId = searchReqId,
                    ..
                  }

    mkWalkGetFareReq :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, JL.JourneyLeg WalkLegRequest m) => m WalkLegRequest
    mkWalkGetFareReq = do
      return $
        (WalkLegRequestGetFare WalkLegRequestGetFareData)

    mkRouteDetails :: Maybe Spec.ServiceTierType -> EMInterface.MultiModalRouteDetails -> Maybe FRFSRouteDetails
    mkRouteDetails serviceTier routeDetails =
      let mbRouteCode = gtfsIdtoDomainCode <$> routeDetails.gtfsId
          mbFromStationCode = gtfsIdtoDomainCode <$> ((routeDetails.fromStopDetails >>= (.stopCode)) <|> gtfsIdtoDomainCode <$> (routeDetails.fromStopDetails >>= (.gtfsId)))
          mbToStationCode = gtfsIdtoDomainCode <$> ((routeDetails.toStopDetails >>= (.stopCode)) <|> gtfsIdtoDomainCode <$> (routeDetails.toStopDetails >>= (.gtfsId)))
       in case (mbRouteCode, mbFromStationCode, mbToStationCode) of
            (Just routeCode, Just startStationCode, Just endStationCode) ->
              Just $ FRFSRouteDetails {routeCode = Just routeCode, serviceTier = serviceTier, ..}
            _ -> Nothing

confirm :: JL.ConfirmFlow m r c => Bool -> Bool -> JL.LegInfo -> Maybe CrisSdkResponse -> [FRFSCategorySelectionReq] -> Maybe Bool -> Maybe Bool -> Maybe Bool -> m ()
confirm forcedBooked bookLater JL.LegInfo {..} crisSdkResponse categorySelectionReq isSingleMode mbEnableOffer mbIsMockPayment =
  case travelMode of
    DTrip.Taxi -> do
      confirmReq :: TaxiLegRequest <- mkTaxiLegConfirmReq
      JL.confirm confirmReq
    DTrip.Bus -> do
      confirmReq :: BusLegRequest <- mkBusLegConfirmReq
      JL.confirm confirmReq
    DTrip.Metro -> do
      confirmReq :: MetroLegRequest <- mkMetroLegConfirmReq
      JL.confirm confirmReq
    DTrip.Subway -> do
      case crisSdkResponse >>= (.latency) of
        Just latency -> fork "Push UTS SDK Latencies" $ addGenericLatency "Uts_Sdk_Request_Booking_Latency" (Milliseconds latency)
        Nothing -> pure ()
      confirmReq :: SubwayLegRequest <- mkSubwayLegConfirmReq
      JL.confirm confirmReq
    DTrip.Walk -> do
      let confirmReq :: WalkLegRequest = WalkLegRequestConfirm WalkLegRequestConfirmData
      JL.confirm confirmReq
  where
    mkTaxiLegConfirmReq :: JL.ConfirmFlow m r c => m TaxiLegRequest
    mkTaxiLegConfirmReq = do
      return $
        TaxiLegRequestConfirm $
          TaxiLegRequestConfirmData
            { bookLater = bookLater,
              forcedBooked,
              searchId,
              estimateId = Id <$> pricingId,
              startTime,
              personId,
              merchantId
            }
    mkMetroLegConfirmReq :: JL.ConfirmFlow m r c => m MetroLegRequest
    mkMetroLegConfirmReq = do
      return $
        MetroLegRequestConfirm $
          MetroLegRequestConfirmData
            { bookLater,
              bookingAllowed,
              searchId = Id searchId,
              quoteId = Id <$> pricingId,
              personId,
              merchantId,
              merchantOperatingCityId,
              isSingleMode,
              mbEnableOffer,
              categorySelectionReq,
              mbIsMockPayment
            }
    mkSubwayLegConfirmReq :: JL.ConfirmFlow m r c => m SubwayLegRequest
    mkSubwayLegConfirmReq = do
      return $
        SubwayLegRequestConfirm $
          SubwayLegRequestConfirmData
            { bookLater,
              searchId = Id searchId,
              bookingAllowed,
              quoteId = Id <$> pricingId,
              personId,
              merchantId,
              merchantOperatingCityId,
              crisSdkResponse,
              isSingleMode,
              mbEnableOffer,
              categorySelectionReq,
              mbIsMockPayment
            }
    mkBusLegConfirmReq :: JL.ConfirmFlow m r c => m BusLegRequest
    mkBusLegConfirmReq = do
      return $
        BusLegRequestConfirm $
          BusLegRequestConfirmData
            { bookLater,
              bookingAllowed,
              searchId = Id searchId,
              quoteId = Id <$> pricingId,
              personId,
              merchantId,
              merchantOperatingCityId,
              categorySelectionReq,
              isSingleMode,
              mbEnableOffer,
              mbIsMockPayment
            }
