module Lib.JourneyLeg.Interface where

--import qualified Domain.Types.Common as DTrip
import qualified Domain.Types.JourneyLeg as DJL
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.SearchRequest as DSR
import qualified Kernel.External.MultiModal.Interface as EMInterface
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.JourneyLeg.Types.Bus
import Lib.JourneyLeg.Types.Metro
import Lib.JourneyLeg.Types.Taxi
-- import Lib.JourneyLeg.Taxi
import Lib.JourneyLeg.Types.Walk
import Lib.JourneyModule.Types
import SharedLogic.Search
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

-- data JLRequest
--   = TaxiReq TaxiLegRequest
--   | BusReq BusLegRequest
--   | MetroReq MetroLegRequest
--   | WalkReq WalkLegRequest

-- JourneyLeg MetroLegRequest m =>
-- mkGetFareReq ::  (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTrip.TravelMode -> EMInterface.MultiModalLeg -> m JLRequest
mkTaxiGetFareReq :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> EMInterface.MultiModalLeg -> m TaxiLegRequest
mkTaxiGetFareReq merchantId merchantOperatingCityId leg = do
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

mkBusGetFareReq :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => EMInterface.MultiModalLeg -> m BusLegRequest
mkBusGetFareReq leg = do
  return $
    BusLegRequestGetFare $
      BusLegRequestGetFareData
        { startLocation = leg.startLocation.latLng,
          endLocation = leg.endLocation.latLng
        }

mkMetroGetFareReq :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => EMInterface.MultiModalLeg -> m MetroLegRequest
mkMetroGetFareReq leg = do
  return $
    MetroLegRequestGetFare $
      MetroLegRequestGetFareData
        { startLocation = leg.startLocation.latLng,
          endLocation = leg.endLocation.latLng
        }

mkWalkGetFareReq :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => EMInterface.MultiModalLeg -> m WalkLegRequest
mkWalkGetFareReq _ = do
  return $
    (WalkLegRequestGetFare WalkLegRequestGetFareData)

mkTaxiLegConfirmReq :: LegInfo -> TaxiLegRequest
mkTaxiLegConfirmReq LegInfo {..} =
  TaxiLegRequestConfirm $
    TaxiLegRequestConfirmData
      { skipBooking = skipBooking,
        estimateId = Id <$> legId,
        personId,
        merchantId
      }

-- mkCancelReq :: (Monad m, JourneyLeg JLRequest m) => LegInfo -> m JLRequest
-- mkCancelReq :: JourneyLeg a m => LegInfo -> m a
-- mkCancelReq legInfo =
--   case legInfo.travelMode of
--     DTrip.Taxi -> return $ TaxiLegRequestCancel TaxiLegRequestCancelData
--     _ -> return $ MetroLegRequestCancel MetroLegRequestCancelData

-- mkConfirmReq :: (Monad m, JourneyLeg JLRequest m) => LegInfo -> m JLRequest
-- mkConfirmReq :: JourneyLeg a m => LegInfo -> m a
-- mkConfirmReq legInfo =
--   case legInfo.travelMode of
--     DTrip.Taxi -> return $ mkTaxiLegConfirmReq legInfo
--     DTrip.Metro -> return $ MetroLegRequestConfirm MetroLegRequestConfirmData
--     DTrip.Bus -> return $ BusLegRequestConfirm BusLegRequestConfirmData
--     DTrip.Walk -> return $ WalkLegRequestConfirm WalkLegRequestConfirmData

mkTaxiSearchReq :: DSR.SearchRequest -> DJL.JourneyLeg -> SearchReqLocation -> [SearchReqLocation] -> TaxiLegRequest
mkTaxiSearchReq parentSearchReq journeyLegData origin stops = TaxiLegRequestSearch $ TaxiLegRequestSearchData {..}

-- -- mkGetFareReq ::  (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTrip.TravelMode -> EMInterface.MultiModalLeg -> m JLRequest
-- mkGetFareReq :: JourneyLeg a m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTrip.TravelMode -> EMInterface.MultiModalLeg -> m a
-- mkGetFareReq merchantId merchantOperatingCityId tripMode leg =
--   case tripMode of
--     DTrip.Taxi -> do
--       merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
--       merchantOpCity <- CQMOC.findById merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
--       return $
--           TaxiLegRequestGetFare $
--             TaxiLegRequestGetFareData
--               { startLocation = leg.startLocation.latLng,
--                 endLocation = leg.endLocation.latLng,
--                 distance = leg.distance,
--                 duration = leg.duration,
--                 merchant,
--                 merchantOpCity
--               }
--     DTrip.Bus ->
--       return $
--           BusLegRequestGetFare $
--             BusLegRequestGetFareData
--               { startLocation = leg.startLocation.latLng,
--                 endLocation = leg.endLocation.latLng
--               }
--     DTrip.Metro ->
--       return $
--           MetroLegRequestGetFare $
--             MetroLegRequestGetFareData
--               { startLocation = leg.startLocation.latLng,
--                 endLocation = leg.endLocation.latLng
--               }
--     DTrip.Walk ->
--       return $
--           (WalkLegRequestGetFare WalkLegRequestGetFareData)

-- mkTaxiLegConfirmReq :: LegInfo -> TaxiLegRequest
-- mkTaxiLegConfirmReq LegInfo {..} =
--   TaxiLegRequestConfirm $
--     TaxiLegRequestConfirmData
--       { skipBooking = skipBooking,
--         estimateId = Id <$> legId,
--         personId,
--         merchantId
--       }

-- -- mkCancelReq :: (Monad m, JourneyLeg JLRequest m) => LegInfo -> m JLRequest
-- mkCancelReq :: JourneyLeg a m => LegInfo -> m a
-- mkCancelReq legInfo =
--   case legInfo.travelMode of
--     DTrip.Taxi -> return $ TaxiLegRequestCancel TaxiLegRequestCancelData
--     _ -> return $ MetroLegRequestCancel MetroLegRequestCancelData

-- -- mkConfirmReq :: (Monad m, JourneyLeg JLRequest m) => LegInfo -> m JLRequest
-- mkConfirmReq :: JourneyLeg a m => LegInfo -> m a
-- mkConfirmReq legInfo =
--   case legInfo.travelMode of
--     DTrip.Taxi -> return $ mkTaxiLegConfirmReq legInfo
--     DTrip.Metro -> return $ MetroLegRequestConfirm MetroLegRequestConfirmData
--     DTrip.Bus -> return $ BusLegRequestConfirm BusLegRequestConfirmData
--     DTrip.Walk -> return $ WalkLegRequestConfirm WalkLegRequestConfirmData
