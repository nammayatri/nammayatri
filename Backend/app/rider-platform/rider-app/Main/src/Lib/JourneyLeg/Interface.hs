module Lib.JourneyLeg.Interface where

import Lib.JourneyModule.Types
import API.Types.RiderPlatform.Management.FRFSTicket
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Common as DTrip
import qualified Domain.Types.FRFSSearch as FRFSSR
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.JourneyLeg as DJL
import Domain.Types.Location
import Domain.Types.LocationAddress
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.WalkLegMultimodal as DWalkLeg
import Kernel.External.Maps.Types
import qualified Kernel.External.MultiModal.Interface as EMInterface
import qualified Kernel.External.Maps.Google.MapsClient.Types as Maps
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.JourneyLeg.Types.Bus
import Lib.JourneyLeg.Types.Metro
import Lib.JourneyLeg.Types.Taxi
-- import Lib.JourneyLeg.Taxi
import Lib.JourneyLeg.Types.Walk
import Lib.JourneyLeg.Types
import SharedLogic.Search
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.Station as QStation
import qualified Storage.Queries.Transformers.Booking as QTB
import Lib.JourneyModule.Utils

mkGetFareReq :: JourneyLeg a m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTrip.TravelMode -> EMInterface.MultiModalLeg -> m a
mkGetFareReq merchantId merchantOperatingCityId tripMode leg =
  case tripMode of
    DTrip.Taxi -> do
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
    DTrip.Bus ->
      return $
        BusLegRequestGetFare $
          BusLegRequestGetFareData
            { startLocation = leg.startLocation.latLng,
              endLocation = leg.endLocation.latLng
            }
    DTrip.Metro ->
      return $
        MetroLegRequestGetFare $
          MetroLegRequestGetFareData
            { startLocation = leg.startLocation.latLng,
              endLocation = leg.endLocation.latLng
            }
    DTrip.Walk -> return (WalkLegRequestGetFare WalkLegRequestGetFareData)


mkTaxiLegConfirmReq :: LegInfo -> TaxiLegRequest
mkTaxiLegConfirmReq LegInfo {..} =
  TaxiLegRequestConfirm $
    TaxiLegRequestConfirmData
      { skipBooking = skipBooking,
        estimateId = Id <$> legId,
        personId,
        merchantId
      }

mkCancelReq :: JourneyLeg a m => LegInfo -> m a
mkCancelReq legInfo =
  case legInfo.travelMode of
    DTrip.Taxi -> return $ TaxiLegRequestCancel TaxiLegRequestCancelData
    _ -> return $ MetroLegRequestCancel MetroLegRequestCancelData


mkConfirmReq :: JourneyLeg a m => LegInfo -> m a
mkConfirmReq legInfo =
  case legInfo.travelMode of
    DTrip.Taxi -> return $ mkTaxiLegConfirmReq legInfo
    DTrip.Metro -> return $ MetroLegRequestConfirm MetroLegRequestConfirmData
    DTrip.Bus -> return $ BusLegRequestConfirm BusLegRequestConfirmData
    DTrip.Walk -> return $ WalkLegRequestConfirm WalkLegRequestConfirmData

mkTaxiSearchReq :: DSR.SearchRequest -> DJL.JourneyLeg -> SearchReqLocation -> [SearchReqLocation] -> TaxiLegRequest
mkTaxiSearchReq parentSearchReq journeyLegData origin stops = TaxiLegRequestSearch $ TaxiLegRequestSearchData {..}
