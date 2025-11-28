{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.InvoicePDF where

import Control.Exception (try)
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Text as T
-- import qualified Data.Text.Encoding as TE
import qualified Data.Time as DT
import qualified Domain.Types.Booking.API as DBAPI
import Domain.Types.Location (LocationAPIEntity)
-- import qualified Domain.Types.Extra.Ride as DRide
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude hiding (try)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

-- import Kernel.External.Encryption (decrypt)

-- | HTML escape function to prevent XSS attacks
-- Escapes: < > & " ' to their HTML entity equivalents
escapeHtml :: Text -> Text
escapeHtml = T.concatMap escapeChar
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '"' = "&quot;"
    escapeChar '\'' = "&#x27;"
    escapeChar c = T.singleton c

data InvoiceData = InvoiceData
  { invoiceId :: Text,
    startDate :: UTCTime,
    endDate :: UTCTime,
    customerName :: Text,
    customerEmail :: Maybe Text,
    customerMobile :: Maybe Text,
    merchantName :: Text,
    merchantAddress :: Maybe Text,
    bookings :: [DBAPI.BookingAPIEntity], -- Now using BookingAPIEntity which includes ride data
    totalAmount :: Maybe HighPrecMoney,
    totalRides :: Int
  }
  deriving (Generic)

-- | Generate invoice PDF from booking data
generateInvoicePDF ::
  ( MonadFlow m,
    EncFlow m r
  ) =>
  Text ->
  DP.Person ->
  [DBAPI.BookingAPIEntity] -> -- Now takes BookingAPIEntity directly
  DM.Merchant ->
  UTCTime ->
  UTCTime ->
  m FilePath
generateInvoicePDF invoiceId person bookings merchant startDate endDate = do
  decryptedEmail <- mapM decrypt person.email
  decryptedMobile <- mapM decrypt person.mobileNumber

  let invoiceData =
        InvoiceData
          { invoiceId = invoiceId,
            startDate = startDate,
            endDate = endDate,
            customerName = fromMaybe "" person.firstName <> maybe "" (\ln -> " " <> ln) person.lastName,
            customerEmail = decryptedEmail,
            customerMobile = maskText <$> decryptedMobile,
            merchantName = merchant.name,
            merchantAddress = Just $ show merchant.defaultCity <> ", " <> show merchant.defaultState,
            bookings = bookings, -- Already has ride data in rideList
            totalAmount = calculateTotalAmount bookings,
            totalRides = length bookings
          }

  -- Generate HTML
  let html = generateInvoiceHTML invoiceData

  -- Get temporary directory and create invoice subdirectory
  tempDir <- liftIO getTemporaryDirectory
  let invoiceDir = tempDir </> "nammayatri_invoices"
  liftIO $ createDirectoryIfMissing True invoiceDir

  -- Save HTML file with UTF-8 encoding (important for Rupee symbol ₹ and other Unicode)
  let htmlPath = invoiceDir </> ("invoice_" <> T.unpack invoiceId <> ".html")
      pdfPath = invoiceDir </> ("invoice_" <> T.unpack invoiceId <> ".pdf")

  -- Use BS.writeFile with encodeUtf8 to ensure proper UTF-8 encoding
  liftIO $ BS.writeFile htmlPath (encodeUtf8 html)

  -- Convert HTML to PDF using wkhtmltopdf (fallback to HTML if not available)
  pdfGenerated <- generatePDFFromHTML htmlPath pdfPath

  if pdfGenerated
    then do
      -- Verify PDF file was actually created
      pdfExists <- liftIO $ doesFileExist pdfPath
      if pdfExists
        then do
          logInfo $ "PDF generated and verified successfully: " <> T.pack pdfPath
          return pdfPath
        else do
          logWarning $ "PDF generation succeeded but file not found at: " <> T.pack pdfPath <> ", using HTML instead"
          return htmlPath
    else do
      logWarning "wkhtmltopdf not available or failed, using HTML file instead"
      return htmlPath

-- | Generate PDF from HTML using wkhtmltopdf
generatePDFFromHTML :: (MonadFlow m, Log m) => FilePath -> FilePath -> m Bool
generatePDFFromHTML htmlPath pdfPath = do
  -- Verify input HTML file exists
  htmlExists <- liftIO $ doesFileExist htmlPath
  unless htmlExists $ do
    logError $ "HTML file not found at: " <> T.pack htmlPath

  -- Try to generate PDF with 60 second timeout
  result <-
    liftIO $
      timeout 60000000 $ -- 60 seconds in microseconds
        (try (readProcessWithExitCode "wkhtmltopdf" ["--enable-local-file-access", htmlPath, pdfPath] "") :: IO (Either SomeException (ExitCode, String, String)))

  case result of
    Just (Right (ExitSuccess, _stdout, stderr)) -> do
      logInfo $ "wkhtmltopdf succeeded: " <> T.pack pdfPath
      unless (null stderr) $ logWarning $ "wkhtmltopdf stderr: " <> T.pack stderr
      return True
    Just (Right (exitCode, stdout, stderr)) -> do
      logError $ "wkhtmltopdf failed with exit code: " <> T.pack (show exitCode)
      logError $ "wkhtmltopdf stdout: " <> T.pack stdout
      logError $ "wkhtmltopdf stderr: " <> T.pack stderr
      return False
    Just (Left ex) -> do
      logError $ "wkhtmltopdf exception: " <> T.pack (show ex)
      return False
    Nothing -> do
      logError "wkhtmltopdf timeout after 60 seconds"
      return False

-- | Calculate total amount from bookings
calculateTotalAmount :: [DBAPI.BookingAPIEntity] -> Maybe HighPrecMoney
calculateTotalAmount bookings =
  let amounts = map (\b -> fromIntegral b.estimatedTotalFare) bookings
   in if null amounts then Nothing else Just $ HighPrecMoney (sum amounts)

-- | Generate HTML invoice
generateInvoiceHTML :: InvoiceData -> Text
generateInvoiceHTML InvoiceData {..} =
  let formattedStartDate = T.pack $ DT.formatTime DT.defaultTimeLocale "%b %-d" startDate
      formattedEndDate = T.pack $ DT.formatTime DT.defaultTimeLocale "%b %-d, %Y" endDate
      formattedAmount = maybe "N/A" (\amt -> T.pack $ show $ getHighPrecMoney amt) totalAmount
      -- Escape all user-controlled data to prevent XSS
      safeCustomerName = escapeHtml customerName
      -- Group bookings by date
      groupedBookings = groupBookingsByDate bookings
   in T.concat
        [ "<!DOCTYPE html>",
          "<html>",
          "<head>",
          "<meta charset='UTF-8'>",
          "<title>Your Invoice</title>",
          "<style>",
          invoiceCSS,
          "</style>",
          "</head>",
          "<body>",
          "<div class='invoice-container'>",
          -- Header
          "<div class='header'>",
          "<div class='header-left'>",
          "<h1>Hey ",
          safeCustomerName,
          ", here's your invoice</h1>",
          "<p class='date-range'>",
          T.pack $ show totalRides,
          " rides taken on ",
          formattedStartDate,
          " - ",
          formattedEndDate,
          "</p>",
          "</div>",
          "<div class='header-right'>",
          "<p class='total-label'>Total Fare</p>",
          "<p class='total-amount'>₹",
          formattedAmount,
          "</p>",
          "</div>",
          "</div>",
          -- Rides grouped by date
          T.concat $ map generateDateGroup groupedBookings,
          "</div>",
          "</body>",
          "</html>"
        ]

-- | Group bookings by date
groupBookingsByDate :: [DBAPI.BookingAPIEntity] -> [(Text, [DBAPI.BookingAPIEntity])]
groupBookingsByDate bookings =
  let grouped = List.groupBy (\b1 b2 -> sameDay b1.createdAt b2.createdAt) bookings
      withDates =
        map
          ( \grp -> case grp of
              (booking : _) -> (formatDateHeader booking.createdAt, grp)
              [] -> ("", [])
          )
          grouped
   in withDates
  where
    sameDay t1 t2 =
      let (y1, m1, d1) = DT.toGregorian (DT.utctDay t1)
          (y2, m2, d2) = DT.toGregorian (DT.utctDay t2)
       in (y1, m1, d1) == (y2, m2, d2)
    formatDateHeader time = T.pack $ DT.formatTime DT.defaultTimeLocale "%-d %b, %Y" time

-- | Generate HTML for a date group
generateDateGroup :: (Text, [DBAPI.BookingAPIEntity]) -> Text
generateDateGroup (dateHeader, bookings) =
  T.concat
    [ "<div class='date-section'>",
      "<div class='date-header'>",
      "<span class='date'>",
      dateHeader,
      "</span>",
      "</div>",
      T.concat $ map generateRideCard bookings,
      "</div>"
    ]

-- | Generate ride card matching the screenshot format
generateRideCard :: DBAPI.BookingAPIEntity -> Text
generateRideCard booking =
  let -- Get first ride from rideList (most recent completed ride)
      mbRide = listToMaybe booking.rideList

      -- Time formatting - use ride times if available, fallback to booking times
      pickupTime = fromMaybe booking.rideScheduledTime (mbRide >>= (.rideStartTime))
      dropTime = fromMaybe booking.updatedAt (mbRide >>= (.rideEndTime))
      formattedPickupTime = T.pack $ DT.formatTime DT.defaultTimeLocale "%-I:%M %p" pickupTime
      formattedDropTime = T.pack $ DT.formatTime DT.defaultTimeLocale "%-I:%M %p" dropTime

      -- Location details - build full address from LocationAPIEntity fields
      fromAddress = buildFullAddress booking.fromLocation
      toAddress = maybe "Unknown" buildFullAddress $ getToLocationFromBooking booking

      -- Ride details
      amount = T.pack $ show booking.estimatedTotalFare
      driverName = maybe "N/A" (.driverName) mbRide
      vehicleNumber = maybe "N/A" (.vehicleNumber) mbRide
      rideId = maybe (T.take 10 $ getId booking.id) (.shortRideId.getShortId) mbRide

      -- Escape all user-controlled data
      safeFromAddress = escapeHtml fromAddress
      safeToAddress = escapeHtml toAddress
      safeDriverName = escapeHtml driverName
      safeVehicleNumber = escapeHtml vehicleNumber
      safeRideId = escapeHtml rideId
   in T.concat
        [ "<div class='ride-card'>",
          "<div class='ride-header'>",
          "<span class='ride-fare'>Ride Fare: ₹",
          amount,
          "</span>",
          "</div>",
          -- Pickup location
          "<div class='location-row'>",
          "<span class='dot green-dot'></span>",
          "<div class='location-details'>",
          "<div class='time'>",
          formattedPickupTime,
          "</div>",
          "<div class='address'>",
          safeFromAddress,
          "</div>",
          "</div>",
          "</div>",
          -- Drop location
          "<div class='location-row'>",
          "<span class='dot red-dot'></span>",
          "<div class='location-details'>",
          "<div class='time'>",
          formattedDropTime,
          "</div>",
          "<div class='address'>",
          safeToAddress,
          "</div>",
          "</div>",
          "</div>",
          -- Driver and vehicle details
          "<div class='ride-footer'>",
          "<div class='detail-item'>",
          "<div class='detail-label'>Driver Name</div>",
          "<div class='detail-value'>",
          safeDriverName,
          "</div>",
          "</div>",
          "<div class='detail-item'>",
          "<div class='detail-label'>Vehicle Number</div>",
          "<div class='detail-value'>",
          safeVehicleNumber,
          "</div>",
          "</div>",
          "<div class='detail-item'>",
          "<div class='detail-label'>Ride ID</div>",
          "<div class='detail-value'>",
          safeRideId,
          "</div>",
          "</div>",
          "</div>",
          "</div>"
        ]

-- | Get to location from booking API details
getToLocationFromBooking :: DBAPI.BookingAPIEntity -> Maybe LocationAPIEntity
getToLocationFromBooking booking =
  case booking.bookingDetails of
    DBAPI.OneWayAPIDetails details -> Just details.toLocation
    DBAPI.RentalAPIDetails details -> details.stopLocation
    DBAPI.InterCityAPIDetails details -> Just details.toLocation
    DBAPI.AmbulanceAPIDetails details -> Just details.toLocation
    DBAPI.DeliveryAPIDetails details -> Just details.toLocation
    DBAPI.MeterRideAPIDetails details -> details.toLocation
    DBAPI.DriverOfferAPIDetails details -> Just details.toLocation
    DBAPI.OneWaySpecialZoneAPIDetails details -> Just details.toLocation

-- | Build full address from LocationAPIEntity fields
buildFullAddress :: LocationAPIEntity -> Text
buildFullAddress loc =
  let parts =
        catMaybes
          [ loc.door,
            loc.building,
            loc.street,
            loc.area,
            loc.city,
            loc.state
          ]
   in if null parts
        then fromMaybe "Unknown Location" loc.area
        else T.intercalate ", " parts

-- | CSS styles for invoice matching the screenshot
invoiceCSS :: Text
invoiceCSS =
  T.concat
    [ "* { margin: 0; padding: 0; box-sizing: border-box; }",
      "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif; color: #2d2d2d; background: #f7f7f7; padding: 20px; }",
      ".invoice-container { max-width: 800px; margin: 0 auto; background: white; padding: 0; }",
      -- Header styles
      ".header { display: flex; justify-content: space-between; align-items: flex-start; padding: 32px 32px 24px 32px; background: white; }",
      ".header-left h1 { font-size: 22px; font-weight: 500; color: #2d2d2d; margin-bottom: 8px; }",
      ".date-range { font-size: 15px; color: #6b7280; font-weight: 400; }",
      ".header-right { text-align: right; }",
      ".total-label { font-size: 15px; color: #6b7280; margin-bottom: 4px; }",
      ".total-amount { font-size: 28px; font-weight: 600; color: #2d2d2d; }",
      -- Date section
      ".date-section { margin-bottom: 0; }",
      ".date-header { padding: 16px 32px; background: #f9fafb; }",
      ".date { font-size: 16px; font-weight: 500; color: #2d2d2d; }",
      ".date-header .ride-fare-header { float: right; font-size: 14px; color: #6b7280; }",
      -- Ride card styles
      ".ride-card { background: white; padding: 24px 32px; border-bottom: 1px solid #e5e7eb; }",
      ".ride-header { margin-bottom: 20px; text-align: right; }",
      ".ride-fare { font-size: 14px; color: #6b7280; }",
      -- Location row styles
      ".location-row { display: flex; align-items: flex-start; margin-bottom: 20px; position: relative; }",
      ".location-row:last-of-type { margin-bottom: 24px; }",
      ".location-row:not(:last-of-type)::after { content: ''; position: absolute; left: 7px; top: 20px; bottom: -20px; width: 2px; background: #e5e7eb; }",
      ".dot { width: 16px; height: 16px; border-radius: 50%; flex-shrink: 0; margin-right: 16px; margin-top: 2px; z-index: 1; position: relative; }",
      ".green-dot { background-color: #10b981; }",
      ".red-dot { background-color: #ef4444; }",
      ".location-details { flex: 1; }",
      ".time { font-size: 15px; font-weight: 600; color: #2d2d2d; margin-bottom: 4px; }",
      ".address { font-size: 14px; color: #6b7280; line-height: 1.5; }",
      -- Footer details
      ".ride-footer { display: flex; justify-content: space-between; padding-top: 16px; border-top: 1px solid #e5e7eb; }",
      ".detail-item { flex: 1; }",
      ".detail-label { font-size: 12px; color: #9ca3af; margin-bottom: 4px; text-transform: uppercase; letter-spacing: 0.5px; }",
      ".detail-value { font-size: 14px; color: #2d2d2d; font-weight: 500; }"
    ]
