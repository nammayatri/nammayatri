module Domain.Action.Dashboard.AppManagement.Parking
  ( listParkingClusters,
    getParkingCityRevenue,
    getParkingCluster,
    setParkingClusterStatus,
    getParkingOccupancy,
    getParkingClusterRevenue,
    verifyParkingTicket,
    cashBookParking,
  )
where

import qualified API.Types.Dashboard.AppManagement.Parking as API
import qualified API.Types.UI.TicketService as UITicket
import Data.List (nub, partition)
import qualified Data.Map.Strict as Map
import qualified Data.Time
import qualified Domain.Action.UI.TicketService as TicketService
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.ServiceCategory as Fields
import qualified Domain.Types.TicketBooking as DTTB
import qualified Domain.Types.TicketBookingService as DTB
import qualified "this" Domain.Types.TicketPlace as DTP
import qualified Domain.Types.TicketService as DTS
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.TimeWindowBooking as TWB
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.TicketBooking as QTB
import qualified Storage.Queries.TicketBookingService as QTBS
import qualified Storage.Queries.TicketPlace as QTicketPlace
import qualified Storage.Queries.TicketService as QTicketService
import Tools.Error

operatingCity :: ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow DMOC.MerchantOperatingCity
operatingCity merchantShortId opCity =
  CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
    >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)

-- | A cluster is a Parking ticket place; its zones are its TimeWindow services.
zonesOf :: DTP.TicketPlace -> Environment.Flow [DTS.TicketService]
zonesOf cluster = filter ((== DTS.TimeWindow) . (.bookingMode)) <$> QTicketService.getTicketServicesByPlaceId cluster.id.getId

clusterInCity :: DMOC.MerchantOperatingCity -> Id DTP.TicketPlace -> Environment.Flow DTP.TicketPlace
clusterInCity city clusterId = do
  cluster <- QTicketPlace.findById clusterId >>= fromMaybeM (InvalidRequest "Parking cluster not found")
  unless (cluster.merchantOperatingCityId == city.id && cluster.placeType == DTP.Parking) $ throwError $ InvalidRequest "Parking cluster not found"
  pure cluster

-- | The path's clusterId is what the dashboard scoped the caller to, so the zone must belong to that cluster.
zoneInCluster :: DMOC.MerchantOperatingCity -> Id DTP.TicketPlace -> Id DTS.TicketService -> Environment.Flow (DTP.TicketPlace, DTS.TicketService)
zoneInCluster city clusterId zoneId = do
  cluster <- clusterInCity city clusterId
  zone <- QTicketService.findById zoneId >>= fromMaybeM (TicketServiceNotFound zoneId.getId)
  unless (zone.placesId == cluster.id.getId && zone.bookingMode == DTS.TimeWindow) $ throwError $ InvalidRequest "Parking zone not found in this cluster"
  pure (cluster, zone)

mkClusterResp :: DTP.TicketPlace -> Environment.Flow API.ParkingClusterResp
mkClusterResp cluster = do
  zones <- zonesOf cluster
  pure API.ParkingClusterResp {id = cluster.id, name = cluster.name, geomGeoJson = cluster.geomGeoJson, isActive = cluster.status == DTP.Active, zoneIds = (.id) <$> zones}

parkingClusters :: DMOC.MerchantOperatingCity -> Environment.Flow [DTP.TicketPlace]
parkingClusters city = filter ((== DTP.Parking) . (.placeType)) <$> QTicketPlace.getTicketPlaces city.id

listParkingClusters :: ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow [API.ParkingClusterResp]
listParkingClusters merchantShortId opCity = mapM mkClusterResp =<< parkingClusters =<< operatingCity merchantShortId opCity

getParkingCluster :: ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Id DTP.TicketPlace -> Environment.Flow API.ParkingClusterResp
getParkingCluster merchantShortId opCity clusterId = do
  city <- operatingCity merchantShortId opCity
  mkClusterResp =<< clusterInCity city clusterId

-- | Take a cluster live (Active: riders see and book its open zones) or back to draft (Unpublished: staff only).
setParkingClusterStatus :: ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Id DTP.TicketPlace -> API.ParkingClusterStatusReq -> Environment.Flow API.ParkingClusterResp
setParkingClusterStatus merchantShortId opCity clusterId req = do
  city <- operatingCity merchantShortId opCity
  cluster <- clusterInCity city clusterId
  let updated = cluster {DTP.status = if req.isActive then DTP.Active else DTP.Unpublished}
  QTicketPlace.updateByPrimaryKey updated
  mkClusterResp updated

-- | A zone right now: per vehicle type, spaces taken by paid stays and by unpaid holds, and the stays parked now.
getParkingOccupancy :: ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Id DTP.TicketPlace -> Id DTS.TicketService -> Environment.Flow API.ParkingOccupancyResp
getParkingOccupancy merchantShortId opCity clusterId zoneId = do
  city <- operatingCity merchantShortId opCity
  (_, zone) <- zoneInCluster city clusterId zoneId
  tz <- TicketService.cityTimeZone city.id
  now <- getCurrentTime
  let lookbackDays = (TWB.effectiveMaxMinutes zone.maxDurationMinutes + 1439) `div` 1440
      today = TWB.localDay tz now
      coversNow r = maybe False (\(s, e) -> s <= now && now < e) ((,) <$> r.startTime <*> r.expiryDate)
  rows <- filter coversNow <$> QTBS.findTimeWindowCandidatesByServiceId zone.id [Data.Time.addDays (negate (fromIntegral lookbackDays) - 1) today .. today]
  let (unpaid, paid) = partition ((== DTB.Pending) . (.status)) rows
      held = filter ((> addUTCTime (-300) now) . (.createdAt)) unpaid
  vehicleTypes <- TicketService.parkingVehicleTypes zone
  let perType =
        [ API.ParkingVehicleTypeOccupancy
            { categoryId = category.id,
              name = category.name,
              capacity = category.availableSeats,
              occupiedNow = length (filter inType paid),
              heldUnpaid = length (filter inType held),
              businessHourId,
              peopleCategoryId
            }
          | (businessHourId, category, peopleCategoryId) <- vehicleTypes,
            let inType r = r.serviceCategoryId == Just category.id
        ]
  stays <- fmap catMaybes . forM (nub [fromMaybe r.id r.parentBookingServiceId | r <- paid]) $ \rootId -> do
    root <- QTBS.findById rootId >>= fromMaybeM (InternalError $ "Parking booking missing: " <> rootId.getId)
    booking <- QTB.findById root.ticketBookingId >>= fromMaybeM (InternalError $ "Ticket booking missing: " <> root.ticketBookingId.getId)
    mbStay <- TicketService.parkingStay root
    pure $ do
      TicketService.ParkingStay {stayStart, stayEnd, stayPaid} <- mbStay
      vehicleNumber <- root.vehicleNumber
      pure
        API.ParkingOperatorStay
          { bookingServiceId = root.id,
            categoryId = root.serviceCategoryId,
            vehicleNumber,
            startTime = stayStart,
            endTime = stayEnd,
            amountPaid = stayPaid,
            paymentMethod = booking.paymentMethod,
            verificationCount = root.verificationCount
          }
  pure API.ParkingOccupancyResp {serviceId = zone.id, name = zone.service, vehicleTypes = perType, stays}

getParkingCityRevenue :: ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Time.Day -> Data.Time.Day -> Environment.Flow [API.ParkingZoneRevenue]
getParkingCityRevenue merchantShortId opCity from to = do
  city <- operatingCity merchantShortId opCity
  parkingRevenue from to =<< parkingClusters city

getParkingClusterRevenue :: ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Id DTP.TicketPlace -> Data.Time.Day -> Data.Time.Day -> Environment.Flow [API.ParkingZoneRevenue]
getParkingClusterRevenue merchantShortId opCity clusterId from to = do
  city <- operatingCity merchantShortId opCity
  cluster <- clusterInCity city clusterId
  parkingRevenue from to [cluster]

-- | Paid parking per zone for stays starting on [from, to] (city-local dates), extensions included, split by payment method.
parkingRevenue :: Data.Time.Day -> Data.Time.Day -> [DTP.TicketPlace] -> Environment.Flow [API.ParkingZoneRevenue]
parkingRevenue from to clusters = do
  when (to < from || Data.Time.diffDays to from > 30) $ throwError $ InvalidRequest "from..to must be a range of at most 31 days"
  fmap concat . forM clusters $ \cluster -> do
    zones <- zonesOf cluster
    forM zones $ \zone -> do
      paid <- filter ((/= DTB.Pending) . (.status)) <$> QTBS.findTimeWindowCandidatesByServiceId zone.id [from .. to]
      -- shortcut: one booking lookup per paid booking in the range; aggregate in SQL if ranges get large.
      methods <- Map.fromList <$> mapM (\bid -> (bid,) . (>>= (.paymentMethod)) <$> QTB.findById bid) (nub ((.ticketBookingId) <$> paid))
      let total rows = sum ((.amount.amount) <$> rows)
          cash = filter (\r -> Map.lookup r.ticketBookingId methods == Just (Just DTTB.CASH)) paid
      pure
        API.ParkingZoneRevenue
          { serviceId = zone.id,
            name = zone.service,
            placeId = cluster.id,
            paidBookings = length (filter (isNothing . (.parentBookingServiceId)) paid),
            paidExtensions = length (filter (isJust . (.parentBookingServiceId)) paid),
            onlineAmount = total paid - total cash,
            cashAmount = total cash,
            totalAmount = total paid
          }

-- | Marshal check of a ticket shown at a zone of the cluster; the ticket must be for this zone.
verifyParkingTicket ::
  ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id DTP.TicketPlace ->
  Id DTS.TicketService ->
  ShortId DTB.TicketBookingService ->
  UITicket.TicketServiceVerificationReq ->
  Environment.Flow UITicket.TicketServiceVerificationResp
verifyParkingTicket merchantShortId opCity clusterId zoneId ticketShortId req = do
  city <- operatingCity merchantShortId opCity
  void $ zoneInCluster city clusterId zoneId
  bookingService <- QTBS.findByShortId ticketShortId >>= fromMaybeM (InvalidRequest "Ticket not found")
  unless (bookingService.ticketServiceId == zoneId) $ throwError $ InvalidRequest "Ticket is not for this zone"
  TicketService.verifyTicketBookingV2 True zoneId ticketShortId req

-- | Walk-in booking (cash, or an online order on the driver's account) taken by a marshal at a zone of the cluster.
cashBookParking ::
  ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id DTP.TicketPlace ->
  Id DTS.TicketService ->
  UITicket.DirectTicketBookingReq ->
  Environment.Flow UITicket.DirectTicketBookingResp
cashBookParking merchantShortId opCity clusterId zoneId req = do
  city <- operatingCity merchantShortId opCity
  void $ zoneInCluster city clusterId zoneId
  unless (all ((== zoneId) . (.serviceId)) req.services) $ throwError $ InvalidRequest "Every service in the request must be this zone"
  requestorId <- asks (.actorInfo.actorId)
  TicketService.postTicketPlacesDirectBook (Nothing, city.merchantId) requestorId clusterId req
