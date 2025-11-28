{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.InvoiceGeneration where

import qualified Data.Text as T
import qualified Data.Time as DT
import Data.Time.Calendar (toGregorian)
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.API as DBAPI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Email.AWS.Flow as Email
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Type as SLT
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC
import qualified Storage.Queries.BookingExtra as QBE
import qualified Storage.Queries.Person as QPerson
import System.Directory (doesFileExist)
import Tools.Error
import Tools.InvoicePDF as PDF

-- | Request types
data GenerateInvoiceReq = GenerateInvoiceReq
  { startDate :: UTCTime,
    endDate :: UTCTime,
    rideTypes :: Maybe [RideType],
    billingCategories :: Maybe [BillingCategory],
    email :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data RideType
  = NORMAL
  | RENTAL
  | INTERCITY
  | AMBULANCE
  | DELIVERY
  | METER_RIDE
  deriving (Generic, Show, Read, FromJSON, ToJSON, ToSchema, Eq)

data BillingCategory
  = BUSINESS
  | PERSONAL
  deriving (Generic, Show, Read, FromJSON, ToJSON, ToSchema, Eq)

-- | Response types
data GenerateInvoiceRes = GenerateInvoiceRes
  { invoiceId :: Text,
    totalBookings :: Int,
    totalAmount :: Maybe HighPrecMoney,
    status :: InvoiceStatus,
    message :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data InvoiceStatus = PROCESSING | COMPLETED | FAILED
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq)

-- | Main invoice generation function
generateInvoice ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  GenerateInvoiceReq ->
  m GenerateInvoiceRes
generateInvoice (personId, merchantId) req@GenerateInvoiceReq {..} = do
  -- Step 1: Validate the request
  validateInvoiceRequest req

  -- Step 2: Get person details
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  -- Step 3: Get bookings based on filters (only COMPLETED bookings)
  allBookings <- QBE.findBookingsForInvoice personId startDate endDate

  -- Step 4: Filter by ride type and billing category in Haskell
  let rideTypeFiltered = case rideTypes of
        Just types -> filter (matchesRideType types) allBookings
        Nothing -> allBookings

      bookings = case billingCategories of
        Just categories -> filter (matchesBillingCategory categories) rideTypeFiltered
        Nothing -> rideTypeFiltered

  when (null bookings) $
    throwError $ InvalidRequest "No bookings found for the given criteria"

  -- Step 5: Generate unique invoice ID
  invoiceIdShort <- generateShortId
  let invoiceId = Id invoiceIdShort.getShortId

  -- Step 6: Calculate total amount from raw bookings (before conversion)
  let totalAmount = calculateTotalAmountFromBookings bookings
      totalCount = length bookings

  -- Step 7: Fork async job to generate PDF and email (uses email from request)
  fork "generate_and_email_invoice" $ do
    generateAndEmailInvoice invoiceId person bookings merchantId email
      `catch` \(e :: SomeException) ->
        logError $ "Invoice generation failed for " <> invoiceId.getId <> ": " <> show e <> "personId: " <> personId.getId <> "merchantId: " <> merchantId.getId <> "email: " <> email
  -- Step 8: Return immediate response
  logInfo $ "Invoice generation initiated for person: " <> personId.getId <> ", invoiceId: " <> invoiceId.getId <> ", total bookings: " <> show totalCount

  return $
    GenerateInvoiceRes
      { invoiceId = invoiceId.getId,
        totalBookings = totalCount,
        totalAmount = totalAmount,
        status = PROCESSING,
        message = "Invoice generation in progress. You will receive it via email at " <> email <> " shortly."
      }

-- | Validation logic
validateInvoiceRequest ::
  (MonadFlow m) =>
  GenerateInvoiceReq ->
  m ()
validateInvoiceRequest GenerateInvoiceReq {..} = do
  now <- getCurrentTime

  let (currentYear, _, _) = toGregorian $ DT.utctDay now
      startYear = (\(y, _, _) -> y) $ toGregorian $ DT.utctDay startDate
      endYear = (\(y, _, _) -> y) $ toGregorian $ DT.utctDay endDate

      daysDiff = diffUTCTime endDate startDate
      daysCount = daysDiff / DT.nominalDay

  -- Validate: Date range can't be more than 30 days
  when (daysCount > 30) $
    throwError $ InvalidRequest "Date range cannot exceed 30 days"

  -- Validate: Start date must be from current year
  when (startYear /= currentYear) $
    throwError $ InvalidRequest "Start date must be from current year"

  -- Validate: End date must be from current year
  when (endYear /= currentYear) $
    throwError $ InvalidRequest "End date must be from current year"

  -- Validate: Start date should be before end date
  when (startDate >= endDate) $
    throwError $ InvalidRequest "Start date must be before end date"

  -- Validate: Dates shouldn't be in future
  when (endDate > now) $
    throwError $ InvalidRequest "End date cannot be in the future"

-- | Convert RideType to BookingDetails pattern (for querying)
rideTypeToBookingDetailsPattern :: RideType -> Text
rideTypeToBookingDetailsPattern = \case
  NORMAL -> "OneWayDetails"
  RENTAL -> "RentalDetails"
  INTERCITY -> "InterCityDetails"
  AMBULANCE -> "AmbulanceDetails"
  DELIVERY -> "DeliveryDetails"
  METER_RIDE -> "MeterRideDetails"

-- | Map BillingCategory to domain type
mapBillingCategory :: BillingCategory -> Text
mapBillingCategory = \case
  BUSINESS -> "BUSINESS"
  PERSONAL -> "PERSONAL"

-- | Check if booking matches any of the specified ride types
matchesRideType :: [RideType] -> DRB.Booking -> Bool
matchesRideType types booking =
  let bookingRideType = getRideTypeFromBookingDetails booking.bookingDetails
   in bookingRideType `elem` types

-- | Check if booking matches any of the specified billing categories
matchesBillingCategory :: [BillingCategory] -> DRB.Booking -> Bool
matchesBillingCategory categories booking =
  let bookingCategory = getBillingCategoryFromBooking booking.billingCategory
   in bookingCategory `elem` categories

-- | Calculate total amount from raw bookings
calculateTotalAmountFromBookings :: [DRB.Booking] -> Maybe HighPrecMoney
calculateTotalAmountFromBookings bookings =
  let amounts = map (getHighPrecMoney . (.estimatedTotalFare.amount)) bookings
   in if null amounts then Nothing else Just $ HighPrecMoney (sum amounts)

-- | Convert SharedLogic.Type.BillingCategory to our BillingCategory type
getBillingCategoryFromBooking :: SLT.BillingCategory -> BillingCategory
getBillingCategoryFromBooking = \case
  SLT.BUSINESS -> BUSINESS
  SLT.PERSONAL -> PERSONAL

-- | Extract ride type from booking details
getRideTypeFromBookingDetails :: DRB.BookingDetails -> RideType
getRideTypeFromBookingDetails = \case
  DRB.OneWayDetails _ -> NORMAL
  DRB.RentalDetails _ -> RENTAL
  DRB.InterCityDetails _ -> INTERCITY
  DRB.AmbulanceDetails _ -> AMBULANCE
  DRB.DeliveryDetails _ -> DELIVERY
  DRB.MeterRideDetails _ -> METER_RIDE
  DRB.DriverOfferDetails _ -> NORMAL
  DRB.OneWaySpecialZoneDetails _ -> NORMAL

-- | Generate PDF and email invoice (async job)
generateAndEmailInvoice ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EncFlow m r,
    Log m
  ) =>
  Id Text ->
  DP.Person ->
  [DRB.Booking] ->
  Id DM.Merchant ->
  Text ->
  m ()
generateAndEmailInvoice invoiceId person bookings merchantId email = do
  -- Get merchant details
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)

  -- Get rider config to fetch email configuration
  merchantOperatingCityId <- case bookings of
    (firstBooking : _) -> return firstBooking.merchantOperatingCityId
    [] -> throwError $ InvalidRequest "No bookings provided for invoice generation" -- This should never happen as we validate bookings exist
  riderConfig <- CQRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)

  -- Get fromEmail from rider config (use emailOtpConfig if available, otherwise default)
  let fromEmail = maybe "noreply@nammayatri.in" (.fromEmail) riderConfig.emailOtpConfig

  -- Get the date range from the first and last booking
  let actualStartDate = minimum $ map (.createdAt) bookings
      actualEndDate = maximum $ map (.createdAt) bookings

  -- Convert bookings to BookingAPIEntity (includes ride data)
  bookingAPIEntities <- mapM (`DBAPI.buildBookingAPIEntity` person.id) bookings

  -- Generate PDF
  pdfPath <- PDF.generateInvoicePDF invoiceId.getId person bookingAPIEntities merchant actualStartDate actualEndDate

  -- Verify file exists before attempting to send
  fileExists <- liftIO $ doesFileExist pdfPath
  unless fileExists $ do
    logError $ "Generated invoice file not found at path: " <> T.pack pdfPath
    throwError $ InternalError "Invoice file generation failed - file not found"

  -- Send email with PDF attachment
  let subject = "Your Namma Yatri Invoice - " <> invoiceId.getId
      bodyText = buildInvoiceEmailBody invoiceId.getId
      fileName = "invoice_" <> invoiceId.getId <> ".pdf"

  liftIO $ Email.sendEmailWithAttachment fromEmail [email] subject bodyText pdfPath fileName

  logInfo $ "Invoice PDF generated and emailed successfully for invoiceId: " <> invoiceId.getId

-- | Build plain text email body for invoice
buildInvoiceEmailBody :: Text -> Text
buildInvoiceEmailBody invoiceId =
  T.unlines
    [ "Dear Customer,",
      "",
      "Please find attached your Namma Yatri invoice (" <> invoiceId <> ") for the requested period.",
      "",
      "The invoice contains details of all your rides during the selected date range.",
      "",
      "If you have any questions or concerns, please contact our support team.",
      "",
      "Thank you for choosing Namma Yatri!",
      "",
      "Best regards,",
      "Namma Yatri Team"
    ]
