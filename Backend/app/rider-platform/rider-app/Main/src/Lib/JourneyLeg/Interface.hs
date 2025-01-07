module Lib.JourneyLeg.Interface where

import qualified Domain.Types.JourneyLeg as DJL
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Trip as DTrip
import qualified Kernel.External.MultiModal.Interface as EMInterface
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.JourneyLeg.Bus ()
import Lib.JourneyLeg.Metro ()
import Lib.JourneyLeg.Taxi ()
import Lib.JourneyLeg.Types.Bus
import Lib.JourneyLeg.Types.Metro
import Lib.JourneyLeg.Types.Taxi
import Lib.JourneyLeg.Types.Walk
import Lib.JourneyLeg.Walk ()
import qualified Lib.JourneyModule.Types as JL
import SharedLogic.Search
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

getFare ::
  JL.GetFareFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  EMInterface.MultiModalLeg ->
  DTrip.TravelMode ->
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

    mkWalkGetFareReq :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, JL.JourneyLeg WalkLegRequest m) => m WalkLegRequest
    mkWalkGetFareReq = do
      return $
        (WalkLegRequestGetFare WalkLegRequestGetFareData)

-- mkTaxiLegConfirmReq :: JL.LegInfo -> TaxiLegRequest
-- mkTaxiLegConfirmReq JL.LegInfo {..} =
--   TaxiLegRequestConfirm $
--     TaxiLegRequestConfirmData
--       { skipBooking = skipBooking,
--         estimateId = Id <$> legId,
--         personId,
--         merchantId
--       }

-- mkCancelReq :: (Monad m, JL.JourneyLeg JLRequest m) => JL.LegInfo -> m JLRequest
-- mkCancelReq :: JL.JourneyLeg a m => JL.LegInfo -> m a
-- mkCancelReq legInfo =
--   case legInfo.travelMode of
--     DTrip.Taxi -> return $ TaxiLegRequestCancel TaxiLegRequestCancelData
--     _ -> return $ MetroLegRequestCancel MetroLegRequestCancelData

-- mkConfirmReq :: (Monad m, JL.JourneyLeg JLRequest m) => JL.LegInfo -> m JLRequest
-- mkConfirmReq :: JL.JourneyLeg a m => JL.LegInfo -> m a
-- mkConfirmReq legInfo =
--   case legInfo.travelMode of
--     DTrip.Taxi -> return $ mkTaxiLegConfirmReq legInfo
--     DTrip.Metro -> return $ MetroLegRequestConfirm MetroLegRequestConfirmData
--     DTrip.Bus -> return $ BusLegRequestConfirm BusLegRequestConfirmData
--     DTrip.Walk -> return $ WalkLegRequestConfirm WalkLegRequestConfirmData

mkTaxiSearchReq :: DSR.SearchRequest -> DJL.JourneyLeg -> SearchReqLocation -> [SearchReqLocation] -> TaxiLegRequest
mkTaxiSearchReq parentSearchReq journeyLegData origin stops = TaxiLegRequestSearch $ TaxiLegRequestSearchData {..}

-- -- mkCancelReq :: (Monad m, JL.JourneyLeg JLRequest m) => JL.LegInfo -> m JLRequest
-- mkCancelReq :: JL.JourneyLeg a m => JL.LegInfo -> m a
-- mkCancelReq legInfo =
--   case legInfo.travelMode of
--     DTrip.Taxi -> return $ TaxiLegRequestCancel TaxiLegRequestCancelData
--     _ -> return $ MetroLegRequestCancel MetroLegRequestCancelData

-- mkConfirmReq :: JL.JourneyLeg a m => JL.LegInfo -> m a
-- mkConfirmReq legInfo =
--   case legInfo.travelMode of
--     DTrip.Taxi -> return $ mkTaxiLegConfirmReq legInfo
--     DTrip.Metro -> return $ MetroLegRequestConfirm MetroLegRequestConfirmData
--     DTrip.Bus -> return $ BusLegRequestConfirm BusLegRequestConfirmData
--     DTrip.Walk -> return $ WalkLegRequestConfirm WalkLegRequestConfirmData
