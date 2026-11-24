module Storage.Queries.FRFSTicketBookingExtra where

import qualified BecknV2.FRFS.Enums as Spec
import Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBookingStatus
import qualified Domain.Types.JourneyLeg as DJL
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Kernel.Beam.Functions
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Id
import qualified Kernel.Types.Price
import Kernel.Utils.Common
import qualified Kernel.Utils.Version
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketBooking as Beam
import Storage.Queries.OrphanInstances.FRFSTicketBooking ()

-- Extra code goes here --

-- | `updateByPrimaryKey` (generated) minus the boarding OTP column. The shuttle pass issue job sets the OTP on its own
-- (`updateBoardingOtp`), so a whole-record write built from an earlier read must not touch that column at all.
-- Keep in step with the generated `updateByPrimaryKey` when columns are added to FRFSTicketBooking.
updateByPrimaryKeyExceptBoardingOtp :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ()
updateByPrimaryKeyExceptBoardingOtp (Domain.Types.FRFSTicketBooking.FRFSTicketBooking {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam._type _type,
      Se.Set Beam.bookingAuthCode bookingAuthCode,
      Se.Set Beam.bppBankAccountNumber bppBankAccountNumber,
      Se.Set Beam.bppBankCode bppBankCode,
      Se.Set Beam.bppDelayedInterest bppDelayedInterest,
      Se.Set Beam.bppItemId bppItemId,
      Se.Set Beam.bppOrderId bppOrderId,
      Se.Set Beam.bppPaymentId bppPaymentId,
      Se.Set Beam.bppSubscriberId bppSubscriberId,
      Se.Set Beam.bppSubscriberUrl bppSubscriberUrl,
      Se.Set Beam.busLocationData (Just $ toJSON busLocationData),
      Se.Set Beam.cancellationCharges cancellationCharges,
      Se.Set Beam.cashbackPayoutOrderId cashbackPayoutOrderId,
      Se.Set Beam.cashbackStatus cashbackStatus,
      Se.Set Beam.clientBundleVersion (fmap Kernel.Utils.Version.versionToText clientBundleVersion),
      Se.Set Beam.clientSdkVersion (fmap Kernel.Utils.Version.versionToText clientSdkVersion),
      Se.Set Beam.cloudType cloudType,
      Se.Set Beam.conductorId conductorId,
      Se.Set Beam.customerCancelled customerCancelled,
      Se.Set Beam.discountedTickets discountedTickets,
      Se.Set Beam.driverId driverId,
      Se.Set Beam.driverMobileNumber driverMobileNumber,
      Se.Set Beam.driverName driverName,
      Se.Set Beam.eventDiscountAmount eventDiscountAmount,
      Se.Set Beam.failureReason failureReason,
      Se.Set Beam.finalBoardedDepotNo finalBoardedDepotNo,
      Se.Set Beam.finalBoardedScheduleNo finalBoardedScheduleNo,
      Se.Set Beam.finalBoardedVehicleNumber finalBoardedVehicleNumber,
      Se.Set Beam.finalBoardedVehicleNumberSource finalBoardedVehicleNumberSource,
      Se.Set Beam.finalBoardedVehicleServiceTierType finalBoardedVehicleServiceTierType,
      Se.Set Beam.finalBoardedWaybillId finalBoardedWaybillId,
      Se.Set Beam.frfsTicketBookingPaymentIdForTicketGeneration frfsTicketBookingPaymentIdForTicketGeneration,
      Se.Set Beam.fromStationAddress fromStationAddress,
      Se.Set Beam.fromStationId fromStationCode,
      Se.Set Beam.fromStationName fromStationName,
      Se.Set Beam.fromStationLat (Kernel.External.Maps.Types.lat <$> fromStationPoint),
      Se.Set Beam.fromStationLon (Kernel.External.Maps.Types.lon <$> fromStationPoint),
      Se.Set Beam.fromStopIdx fromStopIdx,
      Se.Set Beam.googleWalletJWTUrl googleWalletJWTUrl,
      Se.Set Beam.holdId holdId,
      Se.Set Beam.integratedBppConfigId (Kernel.Types.Id.getId integratedBppConfigId),
      Se.Set Beam.isBookingCancellable isBookingCancellable,
      Se.Set Beam.isFareChanged isFareChanged,
      Se.Set Beam.isMockPayment isMockPayment,
      Se.Set Beam.isSingleMode isSingleMode,
      Se.Set Beam.isSpotBooking isSpotBooking,
      Se.Set Beam.journeyOnInitDone journeyOnInitDone,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.multimodalSearchRequestId multimodalSearchRequestId,
      Se.Set Beam.ondcOnInitReceived ondcOnInitReceived,
      Se.Set Beam.ondcOnInitReceivedAt ondcOnInitReceivedAt,
      Se.Set Beam.osBuildVersion osBuildVersion,
      Se.Set Beam.osType osType,
      Se.Set Beam.overriddenAmount overriddenAmount,
      Se.Set Beam.overrideAppliedEntityId overrideAppliedEntityId,
      Se.Set Beam.overrideType overrideType,
      Se.Set Beam.parentBookingId (Kernel.Types.Id.getId <$> parentBookingId),
      Se.Set Beam.partnerOrgId (Kernel.Types.Id.getId <$> partnerOrgId),
      Se.Set Beam.partnerOrgTransactionId (Kernel.Types.Id.getId <$> partnerOrgTransactionId),
      Se.Set Beam.payerVpa payerVpa,
      Se.Set Beam.paymentTxnId paymentTxnId,
      Se.Set Beam.providerDescription providerDescription,
      Se.Set Beam.providerId providerId,
      Se.Set Beam.providerName providerName,
      Se.Set Beam.quoteId (Kernel.Types.Id.getId quoteId),
      Se.Set Beam.recentLocationId (Kernel.Types.Id.getId <$> recentLocationId),
      Se.Set Beam.refundAmount refundAmount,
      Se.Set Beam.rescheduleCount rescheduleCount,
      Se.Set Beam.rideId rideId,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.routeCode routeCode,
      Se.Set Beam.routeName routeName,
      Se.Set Beam.routeStationsJson routeStationsJson,
      Se.Set Beam.searchId (Kernel.Types.Id.getId searchId),
      Se.Set Beam.seatSelectionType seatSelectionType,
      Se.Set Beam.serviceTierType serviceTierType,
      Se.Set Beam.settleableAmount settleableAmount,
      Se.Set Beam.sharedCabNoShows (Kernel.Prelude.Just sharedCabNoShows),
      Se.Set Beam.startTime startTime,
      Se.Set Beam.stationsJson stationsJson,
      Se.Set Beam.status status,
      Se.Set Beam.toStationAddress toStationAddress,
      Se.Set Beam.toStationId toStationCode,
      Se.Set Beam.toStationName toStationName,
      Se.Set Beam.toStationLat (Kernel.External.Maps.Types.lat <$> toStationPoint),
      Se.Set Beam.toStationLon (Kernel.External.Maps.Types.lon <$> toStationPoint),
      Se.Set Beam.toStopIdx toStopIdx,
      Se.Set Beam.currency ((Kernel.Prelude.Just . Kernel.Types.Price.getPriceCurrency) totalPrice),
      Se.Set Beam.price (Kernel.Types.Price.getPriceAmount totalPrice),
      Se.Set Beam.tripId tripId,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.vehicleNumber vehicleNumber,
      Se.Set Beam.vehicleTripId vehicleTripId,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.waybillNo waybillNo,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

-- | This city's CONFIRMED shared-cab bookings with no cab, updated in [windowStart, upperBound), paged newest-first on an
-- updatedAt keyset. updatedAt is the degrade time (degradedBoarding touches the booking and nothing else writes a cab-less
-- CONFIRMED booking after it), whereas createdAt can be hours older. Bounded: fixed page limit, window fronted at 3x the horizon.
findSharedCabDegradedCandidates ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  Maybe UTCTime ->
  Maybe Int ->
  m [FRFSTicketBooking]
findSharedCabDegradedCandidates = sharedCabCandidates False

-- | The same window over the bookings that HAVE a cab: a boarding writes updatedAt, so it dates the ride.
findSharedCabPlatedCandidates ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  Maybe UTCTime ->
  Maybe Int ->
  m [FRFSTicketBooking]
findSharedCabPlatedCandidates = sharedCabCandidates True

sharedCabCandidates ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Bool ->
  Id MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  Maybe UTCTime ->
  Maybe Int ->
  m [FRFSTicketBooking]
sharedCabCandidates plated merchantOperatingCityId windowStart upperBound mbCursor limit = do
  findAllWithOptionsKV
    [ Se.And
        ( [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId.getId,
            Se.Is Beam.serviceTierType $ Se.Eq (Just Spec.SHARED_CAB),
            Se.Is Beam.vehicleNumber $ if plated then Se.Not (Se.Eq Nothing) else Se.Eq Nothing,
            Se.Is Beam.status $ Se.Eq DFRFSTicketBookingStatus.CONFIRMED,
            Se.Is Beam.updatedAt $ Se.GreaterThanOrEq windowStart,
            Se.Is Beam.updatedAt $ Se.LessThan upperBound
          ]
            <> [Se.Is Beam.updatedAt $ Se.LessThan (fromJust mbCursor) | isJust mbCursor]
        )
    ]
    (Se.Desc Beam.updatedAt)
    limit
    Nothing

updateFRFSTicketBookingVehicleDataById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Text ->
  Maybe DJL.BusBoardingMethod ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Spec.ServiceTierType ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Id FRFSTicketBooking ->
  m ()
updateFRFSTicketBookingVehicleDataById
  finalBoardedVehicleNumber
  finalBoardedVehicleNumberSource
  finalBoardedWaybillId
  finalBoardedScheduleNo
  finalBoardedDepotNo
  finalBoardedVehicleServiceTierType
  conductorId
  driverId
  driverName
  driverMobileNumber
  bookingId = do
    now <- getCurrentTime
    updateWithKV
      [ Se.Set Beam.finalBoardedVehicleNumber finalBoardedVehicleNumber,
        Se.Set Beam.finalBoardedVehicleNumberSource finalBoardedVehicleNumberSource,
        Se.Set Beam.finalBoardedWaybillId finalBoardedWaybillId,
        Se.Set Beam.finalBoardedScheduleNo finalBoardedScheduleNo,
        Se.Set Beam.finalBoardedDepotNo finalBoardedDepotNo,
        Se.Set Beam.finalBoardedVehicleServiceTierType finalBoardedVehicleServiceTierType,
        Se.Set Beam.conductorId conductorId,
        Se.Set Beam.driverId driverId,
        Se.Set Beam.driverName driverName,
        Se.Set Beam.driverMobileNumber driverMobileNumber,
        Se.Set Beam.updatedAt now
      ]
      [Se.Is Beam.id $ Se.Eq bookingId.getId]

updateFRFSTicketBookingVehicleNumberById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe Text -> Id FRFSTicketBooking -> m ()
updateFRFSTicketBookingVehicleNumberById vehicleNumber bookingId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.vehicleNumber vehicleNumber,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq bookingId.getId]

updateBookingAuthCodeById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe Text -> Id FRFSTicketBooking -> m ()
updateBookingAuthCodeById bookingAuthCode id = do
  now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.bookingAuthCode bookingAuthCode, Se.Set Beam.updatedAt now] [Se.Is Beam.id $ Se.Eq (id.getId)]

insertPayerVpaIfNotPresent :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe Text -> Id FRFSTicketBooking -> m ()
insertPayerVpaIfNotPresent (Just vpa) bookingId = do
  mbBooking <- findOneWithKV [Se.Is Beam.id $ Se.Eq (bookingId.getId)]
  whenJust mbBooking $ \booking -> do
    when (isNothing booking.payerVpa) $ do
      updatePayerVpaByBookingId bookingId vpa
  pure ()
insertPayerVpaIfNotPresent _ _ = pure ()

updatePayerVpaByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id FRFSTicketBooking -> Text -> m ()
updatePayerVpaByBookingId id payerVpa = do
  now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.payerVpa (Just payerVpa), Se.Set Beam.updatedAt now] [Se.Is Beam.id $ Se.Eq (id.getId)]

findAllByCashbackStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe Int -> Maybe Int -> Maybe CashbackStatus -> m [FRFSTicketBooking]
findAllByCashbackStatus limit offset cashbackStatus = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.cashbackStatus $ Se.Eq cashbackStatus,
          Se.Is Beam.payerVpa $ Se.Not $ Se.Eq Nothing
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findAllByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Maybe Int -> Maybe Int -> Id Person -> Maybe Spec.VehicleCategory -> Maybe Text -> m [FRFSTicketBooking]
findAllByRiderId limit offset riderId mbVehicleCategory mbOverrideEntityId = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]
            <> [Se.Is Beam.vehicleType $ Se.Eq (fromJust mbVehicleCategory) | isJust mbVehicleCategory]
            <> [Se.Is Beam.overrideAppliedEntityId $ Se.Eq mbOverrideEntityId | isJust mbOverrideEntityId]
        )
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

-- | A rider's live shared-cab bookings, newest first: filtered in the query so other FRFS bookings can't page them out.
findConfirmedSharedCabByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id Person -> m [FRFSTicketBooking]
findConfirmedSharedCabByRiderId riderId =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId),
          Se.Is Beam.status $ Se.Eq DFRFSTicketBookingStatus.CONFIRMED,
          Se.Is Beam.serviceTierType $ Se.Eq (Just Spec.SHARED_CAB)
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just 5)
    Nothing

findAllByProviderNameAndCreatedAtAfterAndStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> UTCTime -> DFRFSTicketBookingStatus.FRFSTicketBookingStatus -> m [FRFSTicketBooking]
findAllByProviderNameAndCreatedAtAfterAndStatus providerName createdAtAfter status = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.providerName $ Se.Eq providerName,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq createdAtAfter,
          Se.Is Beam.status $ Se.Eq status
        ]
    ]

findAllByTripId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> m [FRFSTicketBooking]
findAllByTripId tripId = do
  findAllWithKV [Se.Is Beam.tripId $ Se.Eq (Just tripId)]

findAllConfirmedByTripId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> m [FRFSTicketBooking]
findAllConfirmedByTripId tripId = do
  findAllWithKV
    [ Se.Is Beam.tripId $ Se.Eq (Just tripId),
      Se.Is Beam.status $ Se.Eq DFRFSTicketBookingStatus.CONFIRMED
    ]

-- | All confirmed bookings on a waybill (across all its trips), via the dedicated waybillNo secondary key
-- (set at booking creation from the tripId). Used to fan a waybill fleet/driver change out to the affected
-- customer tickets.
findAllConfirmedByWaybillNo :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> m [FRFSTicketBooking]
findAllConfirmedByWaybillNo waybillNo = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.waybillNo $ Se.Eq (Just waybillNo),
          Se.Is Beam.status $ Se.Eq DFRFSTicketBookingStatus.CONFIRMED
        ]
    ]

-- | A route's FINDING shared-cab bookings in a city: CONFIRMED with no cab yet. The predicates are ddl 1587's partial index's.
findSharedCabFindingOnRoute ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  Text ->
  UTCTime ->
  m [FRFSTicketBooking]
findSharedCabFindingOnRoute merchantOperatingCityId routeCode since =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.vehicleNumber $ Se.Eq Nothing,
          Se.Is Beam.status $ Se.Eq DFRFSTicketBookingStatus.CONFIRMED,
          Se.Is Beam.serviceTierType $ Se.Eq (Just Spec.SHARED_CAB),
          Se.Is Beam.routeCode $ Se.Eq (Just routeCode),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId.getId,
          Se.Is Beam.updatedAt $ Se.GreaterThanOrEq since
        ]
    ]

-- | A city's FINDING shared-cab bookings touched since `since`: CONFIRMED with no cab yet. The window drops ended degraded rides,
-- which stay CONFIRMED with no cab once their tickets are USED.
findSharedCabFindingInCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  UTCTime ->
  m [FRFSTicketBooking]
findSharedCabFindingInCity merchantOperatingCityId since =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.vehicleNumber $ Se.Eq Nothing,
          Se.Is Beam.status $ Se.Eq DFRFSTicketBookingStatus.CONFIRMED,
          Se.Is Beam.serviceTierType $ Se.Eq (Just Spec.SHARED_CAB),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId.getId,
          Se.Is Beam.updatedAt $ Se.GreaterThanOrEq since
        ]
    ]

-- | A city's CONFIRMED shared-cab bookings that have a cab, touched since `since`: allocated and riding, without the ride history.
findSharedCabPlatedCityUpdatedSince ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id MerchantOperatingCity ->
  UTCTime ->
  m [FRFSTicketBooking]
findSharedCabPlatedCityUpdatedSince merchantOperatingCityId since =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId.getId,
          Se.Is Beam.status $ Se.Eq DFRFSTicketBookingStatus.CONFIRMED,
          Se.Is Beam.serviceTierType $ Se.Eq (Just Spec.SHARED_CAB),
          Se.Is Beam.vehicleNumber $ Se.Not (Se.Eq Nothing),
          Se.Is Beam.updatedAt $ Se.GreaterThanOrEq since
        ]
    ]

-- | The plates' shared-cab bookings in these statuses touched since `since`: the plates' runs without their past.
findSharedCabPlatesUpdatedSince ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Text] ->
  [DFRFSTicketBookingStatus.FRFSTicketBookingStatus] ->
  UTCTime ->
  m [FRFSTicketBooking]
findSharedCabPlatesUpdatedSince [] _ _ = pure []
findSharedCabPlatesUpdatedSince plates statuses since =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.vehicleNumber $ Se.In (Just <$> plates),
          Se.Is Beam.serviceTierType $ Se.Eq (Just Spec.SHARED_CAB),
          Se.Is Beam.status $ Se.In statuses,
          Se.Is Beam.updatedAt $ Se.GreaterThanOrEq since
        ]
    ]

-- | By primary key, so KV answers: a write not yet drained to the DB is seen.
findAllByIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Id FRFSTicketBooking] -> m [FRFSTicketBooking]
findAllByIds [] = pure []
findAllByIds ids = findAllWithKV [Se.Is Beam.id $ Se.In (getId <$> ids)]

-- | A pass term's bookings starting in [dayStart, dayEnd): the shuttle daily issue job's "already issued for day D?"
-- check, so a rerun never issues a second ticket. overrideAppliedEntityId is the PurchasedPassPayment id.
findAllByOverrideAppliedEntityIdAndStartTimeRange ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  UTCTime ->
  UTCTime ->
  m [FRFSTicketBooking]
findAllByOverrideAppliedEntityIdAndStartTimeRange entityId dayStart dayEnd =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.overrideAppliedEntityId $ Se.Eq (Just entityId),
          Se.Is Beam.startTime $ Se.GreaterThanOrEq (Just dayStart),
          Se.Is Beam.startTime $ Se.LessThan (Just dayEnd)
        ]
    ]
