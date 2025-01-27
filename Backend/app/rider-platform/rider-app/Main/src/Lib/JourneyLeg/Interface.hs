module Lib.JourneyLeg.Interface where

import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Trip as DTrip
import qualified Kernel.External.MultiModal.Interface as EMInterface
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
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
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

getFare ::
  JL.GetFareFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  EMInterface.MultiModalLeg ->
  DTrip.MultimodalTravelMode ->
  m (Maybe JL.GetFareResponse)
getFare merchantId merchantOperatingCityId leg = \case
  DTrip.Taxi -> do
    getFareReq :: TaxiLegRequest <- mkTaxiGetFareReq
    JL.getFare getFareReq
  DTrip.Bus -> do
    getFareReq :: BusLegRequest <- mkBusGetFareReq
    JL.getFare getFareReq
  DTrip.Metro -> do
    getFareReq :: MetroLegRequest <- mkMetroGetFareReq
    JL.getFare getFareReq
  DTrip.Subway -> do
    getFareReq :: SubwayLegRequest <- mkSubwayGetFareReq
    JL.getFare getFareReq
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

    mkBusGetFareReq :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, JL.JourneyLeg BusLegRequest m) => m BusLegRequest
    mkBusGetFareReq = do
      return $
        BusLegRequestGetFare $
          BusLegRequestGetFareData
            { startLocation = leg.startLocation.latLng,
              endLocation = leg.endLocation.latLng
            }

    mkMetroGetFareReq :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, JL.JourneyLeg MetroLegRequest m) => m MetroLegRequest
    mkMetroGetFareReq = do
      return $
        MetroLegRequestGetFare $
          MetroLegRequestGetFareData
            { startLocation = leg.startLocation.latLng,
              endLocation = leg.endLocation.latLng
            }

    mkSubwayGetFareReq :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, JL.JourneyLeg SubwayLegRequest m) => m SubwayLegRequest
    mkSubwayGetFareReq = do
      return $
        SubwayLegRequestGetFare $
          SubwayLegRequestGetFareData
            { startLocation = leg.startLocation.latLng,
              endLocation = leg.endLocation.latLng
            }

    mkWalkGetFareReq :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, JL.JourneyLeg WalkLegRequest m) => m WalkLegRequest
    mkWalkGetFareReq = do
      return $
        (WalkLegRequestGetFare WalkLegRequestGetFareData)

confirm :: JL.ConfirmFlow m r c => JL.LegInfo -> m ()
confirm JL.LegInfo {..} =
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
            { skipBooking = skipBooking,
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
            { skipBooking,
              bookingAllowed,
              quoteId = Id <$> pricingId,
              personId,
              merchantId,
              merchantOperatingCityId
            }
    mkSubwayLegConfirmReq :: JL.ConfirmFlow m r c => m SubwayLegRequest
    mkSubwayLegConfirmReq = do
      return $
        SubwayLegRequestConfirm $
          SubwayLegRequestConfirmData
            { skipBooking,
              bookingAllowed,
              quoteId = Id <$> pricingId,
              personId,
              merchantId,
              merchantOperatingCityId
            }
    mkBusLegConfirmReq :: JL.ConfirmFlow m r c => m BusLegRequest
    mkBusLegConfirmReq = do
      return $
        BusLegRequestConfirm $
          BusLegRequestConfirmData
            { skipBooking,
              bookingAllowed,
              quoteId = Id <$> pricingId,
              personId,
              merchantId,
              merchantOperatingCityId
            }
