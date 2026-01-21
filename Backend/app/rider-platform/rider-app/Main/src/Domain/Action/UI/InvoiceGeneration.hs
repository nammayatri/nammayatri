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
import qualified Domain.Types.RideStatus as DRide
import qualified Email.Flow as Email
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Booking as SB
import qualified SharedLogic.Type as SLT
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC
import qualified Storage.Queries.Booking as QBE
import qualified Storage.Queries.Person as QPerson
import System.Directory (doesFileExist)
import Tools.Error
import Tools.InvoicePDF as PDF

-- | Request types
data GenerateInvoiceReq = GenerateInvoiceReq
  { startDate :: UTCTime,
    endDate :: UTCTime,
    rideTypes :: Maybe [SLT.RideType],
    billingCategories :: Maybe [SLT.BillingCategory],
    email :: Maybe Text,
    bookingId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | Response types
data GenerateInvoiceRes = GenerateInvoiceRes
  { invoiceId :: Text,
    totalBookings :: Int,
    totalAmount :: Maybe HighPrecMoney,
    bookingAPIEntities :: [DBAPI.BookingAPIEntity],
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
    CacheFlow m r,
    HasField "emailServiceConfig" r Email.EmailServiceConfig
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  GenerateInvoiceReq ->
  m GenerateInvoiceRes
generateInvoice (personId, merchantId) req@GenerateInvoiceReq {..} = do
  -- Step 1: Validate the request
  validateInvoiceRequest req

  -- Step 2: Get person details
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  -- Step 3: Get bookings based on rideId or filters (only COMPLETED bookings)
  allBookings <- case bookingId of
    Just bookingId' -> do
      booking <- QBE.findCompletedBookingById (Id bookingId')
      return (maybeToList booking)
    Nothing -> do
      QBE.findBookingsForInvoice personId startDate endDate Nothing Nothing

  -- Step 4: Filter by ride type and billing category in Haskell
  let rideTypeFiltered = case rideTypes of
        Just types -> filter (SB.matchesRideType types) allBookings
        Nothing -> allBookings

      bookings = case billingCategories of
        Just categories -> filter (SB.matchesBillingCategory categories) rideTypeFiltered
        Nothing -> rideTypeFiltered

  when (null bookings) $
    throwError $ InvalidRequest "No bookings found for the given criteria"

  -- Step 5: Generate unique invoice ID
  invoiceIdShort <- generateShortId
  let invoiceId = Id invoiceIdShort.getShortId

  -- Convert bookings to BookingAPIEntity (includes ride data)
  allBookingAPIEntities <- mapM (`DBAPI.buildBookingAPIEntity` person.id) bookings

  -- Filter: Keep only bookings with exactly 1 completed ride
  let validBookingAPIEntities = filter hasExactlyOneRide allBookingAPIEntities

  when (null validBookingAPIEntities) $
    throwError $ InvalidRequest "No valid bookings with completed rides found"

  -- Step 6: Calculate total amount from actual ride fares (computedPrice)
  let totalAmount = calculateTotalFromRides validBookingAPIEntities
      totalCount = length validBookingAPIEntities

  logInfo $ "Filtered bookings: " <> show (length allBookingAPIEntities) <> " total, " <> show totalCount <> " valid (with exactly 1 ride)"

  -- Step 7: Fork async job to generate PDF and email (uses email from request)
  whenJust email $ \emaill -> do
    fork "generate_and_email_invoice" $ do
      generateAndEmailInvoice invoiceId person validBookingAPIEntities merchantId emaill
        `catch` \(e :: SomeException) ->
          logError $ "Invoice generation failed for " <> invoiceId.getId <> ": " <> show e <> "personId: " <> personId.getId <> "merchantId: " <> merchantId.getId <> "email: " <> emaill
  -- Step 8: Return immediate response
  logInfo $ "Invoice generation initiated for person: " <> personId.getId <> ", invoiceId: " <> invoiceId.getId <> ", total bookings: " <> show totalCount

  return $
    GenerateInvoiceRes
      { invoiceId = invoiceId.getId,
        totalBookings = totalCount,
        totalAmount = totalAmount,
        status = PROCESSING,
        bookingAPIEntities = validBookingAPIEntities,
        message = "Invoice generation in progress. You will receive it via email at " <> show email <> " shortly."
      }

-- | Validation logic
validateInvoiceRequest ::
  (MonadFlow m) =>
  GenerateInvoiceReq ->
  m ()
validateInvoiceRequest GenerateInvoiceReq {..} = do
  now <- getCurrentTime

  let (currentYear, _, _) = toGregorian $ DT.utctDay now
      _startYear = (\(y, _, _) -> y) $ toGregorian $ DT.utctDay startDate
      endYear = (\(y, _, _) -> y) $ toGregorian $ DT.utctDay endDate

      daysDiff = diffUTCTime endDate startDate
      daysCount = daysDiff / DT.nominalDay

  -- Validate: Date range can't be more than 30 days
  when (daysCount > 31) $
    throwError $ InvalidRequest "Date range cannot exceed 31 days"

  -- Validate: Start date must not be more than 1 year in the past
  let oneYearInSeconds = 365 * DT.nominalDay
      timeSinceStart = diffUTCTime now startDate
  when (timeSinceStart > oneYearInSeconds) $
    throwError $ InvalidRequest "Start date cannot be more than 1 year in the past"

  -- Validate: End date must be from current year
  when (endYear /= currentYear) $
    throwError $ InvalidRequest "End date must be from current year"

  -- Validate: Start date should be before end date
  when (startDate >= endDate) $
    throwError $ InvalidRequest "Start date must be before end date"

  -- Validate: End date shouldn't be in future (compare by date only, not time)
  let todayDate = DT.utctDay now
      endDateDay = DT.utctDay endDate
  when (endDateDay > todayDate) $
    throwError $ InvalidRequest "End date cannot be in the future"

-- | Check if booking has at least one completed ride
hasExactlyOneRide :: DBAPI.BookingAPIEntity -> Bool
hasExactlyOneRide booking =
  let completedRides = filter (\ride -> ride.status == DRide.COMPLETED) booking.rideList
   in length completedRides == 1

-- | Calculate total amount from actual ride computed prices
calculateTotalFromRides :: [DBAPI.BookingAPIEntity] -> Maybe HighPrecMoney
calculateTotalFromRides bookings =
  let amounts = mapMaybe getRideComputedPrice bookings
   in if null amounts then Nothing else Just $ HighPrecMoney (sum amounts)
  where
    getRideComputedPrice :: DBAPI.BookingAPIEntity -> Maybe Rational
    getRideComputedPrice booking = do
      let completedRide = filter (\ride -> ride.status == DRide.COMPLETED) booking.rideList
      ride <- listToMaybe completedRide
      computedPrice <- ride.computedPrice
      return $ fromIntegral computedPrice

-- | Calculate total amount from raw bookings (fallback, not used anymore)
calculateTotalAmountFromBookings :: [DRB.Booking] -> Maybe HighPrecMoney
calculateTotalAmountFromBookings bookings =
  let amounts = map (getHighPrecMoney . (.estimatedTotalFare.amount)) bookings
   in if null amounts then Nothing else Just $ HighPrecMoney (sum amounts)

-- | Generate PDF and email invoice (async job)
generateAndEmailInvoice ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EncFlow m r,
    Log m,
    HasField "emailServiceConfig" r Email.EmailServiceConfig
  ) =>
  Id Text ->
  DP.Person ->
  [DBAPI.BookingAPIEntity] ->
  Id DM.Merchant ->
  Text ->
  m ()
generateAndEmailInvoice invoiceId person bookingAPIEntities merchantId email = do
  -- Get merchant details
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)

  -- Get rider config to fetch email configuration
  merchantOperatingCityId <- case bookingAPIEntities of
    (firstBooking : _) -> return firstBooking.merchantOperatingCityId
    [] -> throwError $ InvalidRequest "No bookings provided for invoice generation" -- This should never happen as we validate bookings exist
  riderConfig <- CQRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)

  -- Get fromEmail from rider config (use emailOtpConfig if available, otherwise default)
  let fromEmail = maybe "noreply@nammayatri.in" (.fromEmail) riderConfig.emailOtpConfig

  -- Get the date range from the first and last booking
  let actualStartDate = minimum $ map (.createdAt) bookingAPIEntities
      actualEndDate = maximum $ map (.createdAt) bookingAPIEntities

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

  emailServiceConfig <- asks (.emailServiceConfig)

  liftIO $ Email.sendEmailWithAttachment emailServiceConfig fromEmail [email] subject bodyText pdfPath fileName

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
