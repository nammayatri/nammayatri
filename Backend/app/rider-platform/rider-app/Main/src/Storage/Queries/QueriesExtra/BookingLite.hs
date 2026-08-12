{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.QueriesExtra.BookingLite where

import qualified Domain.Types.Booking
import qualified Domain.Types.BookingStatus
import qualified Domain.Types.Common
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Quote
import qualified Domain.Types.ServiceTierType
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified SharedLogic.Type
import qualified Storage.Beam.Booking as Beam
import qualified Storage.Queries.Transformers.Booking

---------------- Lightweight Booking read: single KV read, only scalar columns + cheap pure ----------------
---------------- transforms. NO LocationMapping / Location / bookingDetails hydration. Read-only. ----------------
---------------- Add scalar fields below as flows need them.                                     ----------------

findByIdLite :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m (Maybe BookingLite))
findByIdLite id = findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByBPPBookingIdLite :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Booking.BPPBooking -> m (Maybe BookingLite)
findByBPPBookingIdLite (Kernel.Types.Id.Id bppRbId) = findOneWithKV [Se.Is Beam.bppBookingId $ Se.Eq $ Just bppRbId]

-- | My Rides list read: a page of lightweight bookings for a rider, newest first (no location decode).
findAllByRiderIdLite ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  [Domain.Types.BookingStatus.BookingStatus] ->
  m [BookingLite]
findAllByRiderIdLite personId mbLimit mbOffset mbFrom mbTo statusList =
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId personId)]
            <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq f | Just f <- [mbFrom]]
            <> [Se.Is Beam.createdAt $ Se.LessThanOrEq t | Just t <- [mbTo]]
            <> [Se.Is Beam.status $ Se.In statusList | not (null statusList)]
            -- Exclude multimodal journey-leg bookings (they render only in the journey card).
            <> [Se.Or [Se.Is Beam.isMultimodalSearch $ Se.Eq Nothing, Se.Is Beam.isMultimodalSearch $ Se.Eq (Just False)]]
        )
    ]
    (Se.Desc Beam.startTime)
    mbLimit
    mbOffset

data BookingLite = BookingLite
  { id :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    status :: Domain.Types.BookingStatus.BookingStatus,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    providerId :: Kernel.Prelude.Text,
    providerUrl :: Kernel.Prelude.BaseUrl,
    bppBookingId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Booking.BPPBooking),
    transactionId :: Kernel.Prelude.Text,
    vehicleServiceTierType :: Domain.Types.ServiceTierType.ServiceTierType,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    quoteId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Quote.Quote),
    tripCategory :: Kernel.Prelude.Maybe Domain.Types.Common.TripCategory,
    isAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    paymentMode :: Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentMode,
    createdAt :: Kernel.Prelude.UTCTime,
    -- Added for listV3: cheap scalar reads, still no location/bookingDetails hydration.
    startTime :: Kernel.Prelude.UTCTime,
    isScheduled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    serviceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleIconUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    billingCategory :: Kernel.Prelude.Maybe SharedLogic.Type.BillingCategory,
    estimatedFare :: Kernel.Types.Common.HighPrecMoney,
    locationNames :: Kernel.Prelude.Maybe [Kernel.Prelude.Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

type BookingLiteTable = Beam.BookingT Identity

instance FromTType' BookingLiteTable BookingLite where
  fromTType' (Beam.BookingT {..}) = do
    merchantOperatingCityId' <- Storage.Queries.Transformers.Booking.backfillMOCId merchantOperatingCityId merchantId
    providerUrl' <- parseBaseUrl providerUrl
    pure $
      Just
        BookingLite
          { id = Kernel.Types.Id.Id id,
            status = status,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            providerId = providerId,
            providerUrl = providerUrl',
            bppBookingId = Kernel.Types.Id.Id <$> bppBookingId,
            transactionId = riderTransactionId,
            vehicleServiceTierType = vehicleVariant,
            riderId = Kernel.Types.Id.Id riderId,
            quoteId = Kernel.Types.Id.Id <$> quoteId,
            tripCategory = tripCategory,
            isAirConditioned = isAirConditioned,
            paymentMode = paymentMode,
            createdAt = createdAt,
            startTime = startTime,
            isScheduled = isScheduled,
            serviceTierName = serviceTierName,
            vehicleIconUrl = vehicleIconUrl,
            billingCategory = billingCategory,
            estimatedFare = estimatedFare,
            locationNames = locationNames
          }
