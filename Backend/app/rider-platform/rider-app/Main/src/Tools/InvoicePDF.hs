{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.InvoicePDF where

import qualified Data.Text as T
import qualified Data.Time as DT
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import System.Directory (createDirectoryIfMissing)
import System.Process (callCommand)

-- import Kernel.Types.Common (EncFlow)

data InvoiceData = InvoiceData
  { invoiceId :: Text,
    invoiceDate :: UTCTime,
    customerName :: Text,
    customerEmail :: Maybe Text,
    customerMobile :: Maybe Text,
    merchantName :: Text,
    merchantAddress :: Maybe Text,
    bookings :: [DRB.Booking],
    totalAmount :: Maybe HighPrecMoney,
    totalRides :: Int
  }
  deriving (Generic, Show)

-- | Generate invoice PDF from booking data
generateInvoicePDF ::
  ( MonadFlow m,
    EncFlow m r
  ) =>
  Text ->
  DP.Person ->
  [DRB.Booking] ->
  DM.Merchant ->
  m FilePath
generateInvoicePDF invoiceId person bookings merchant = do
  now <- getCurrentTime
  decryptedEmail <- mapM decrypt person.email
  decryptedMobile <- mapM decrypt person.mobileNumber

  let invoiceData =
        InvoiceData
          { invoiceId = invoiceId,
            invoiceDate = now,
            customerName = fromMaybe "" person.firstName <> maybe "" (\ln -> " " <> ln) person.lastName,
            customerEmail = decryptedEmail,
            customerMobile = maskText <$> decryptedMobile,
            merchantName = merchant.name,
            merchantAddress = Just $ show merchant.defaultCity <> ", " <> show merchant.defaultState,
            bookings = bookings,
            totalAmount = calculateTotalAmount bookings,
            totalRides = length bookings
          }

  -- Generate HTML
  let html = generateInvoiceHTML invoiceData

  -- Create assets directory if it doesn't exist
  liftIO $ createDirectoryIfMissing True "src/assets"

  -- Save HTML file
  let htmlPath = "src/assets/invoice_" <> T.unpack invoiceId <> ".html"
      pdfPath = "src/assets/invoice_" <> T.unpack invoiceId <> ".pdf"

  liftIO $ writeFile htmlPath (T.unpack html)

  -- Convert HTML to PDF using wkhtmltopdf (fallback to HTML if not available)
  pdfGenerated <- liftIO $ generatePDFFromHTML htmlPath pdfPath

  if pdfGenerated
    then do
      logInfo $ "PDF generated successfully: " <> T.pack pdfPath
      return pdfPath
    else do
      logWarning "wkhtmltopdf not available, using HTML file instead"
      return htmlPath

-- | Generate PDF from HTML using wkhtmltopdf
generatePDFFromHTML :: FilePath -> FilePath -> IO Bool
generatePDFFromHTML htmlPath pdfPath = do
  -- Try to generate PDF, catch errors if wkhtmltopdf is not installed
  (try :: IO () -> IO (Either SomeException ())) (callCommand $ "wkhtmltopdf " <> htmlPath <> " " <> pdfPath) >>= \case
    Right _ -> return True
    Left _ -> return False

-- | Calculate total amount from bookings
calculateTotalAmount :: [DRB.Booking] -> Maybe HighPrecMoney
calculateTotalAmount bookings =
  let amounts = map (getHighPrecMoney . (.estimatedTotalFare.amount)) bookings
   in if null amounts then Nothing else Just $ HighPrecMoney (sum amounts)

-- | Generate HTML invoice
generateInvoiceHTML :: InvoiceData -> Text
generateInvoiceHTML InvoiceData {..} =
  let formattedDate = T.pack $ DT.formatTime DT.defaultTimeLocale "%B %d, %Y" invoiceDate
      formattedAmount = maybe "N/A" (T.pack . show . getHighPrecMoney) totalAmount
   in T.concat
        [ "<!DOCTYPE html>",
          "<html>",
          "<head>",
          "<meta charset='UTF-8'>",
          "<title>Invoice ",
          invoiceId,
          "</title>",
          "<style>",
          invoiceCSS,
          "</style>",
          "</head>",
          "<body>",
          "<div class='invoice-container'>",
          -- Header
          "<div class='header'>",
          "<div class='logo'>",
          "<h1>",
          merchantName,
          "</h1>",
          maybe "" (\addr -> "<p>" <> addr <> "</p>") merchantAddress,
          "</div>",
          "<div class='invoice-info'>",
          "<h2>INVOICE</h2>",
          "<p><strong>Invoice #:</strong> ",
          invoiceId,
          "</p>",
          "<p><strong>Date:</strong> ",
          formattedDate,
          "</p>",
          "</div>",
          "</div>",
          -- Customer Info
          "<div class='customer-info'>",
          "<h3>Bill To:</h3>",
          "<p><strong>",
          customerName,
          "</strong></p>",
          maybe "" (\email -> "<p>Email: " <> email <> "</p>") customerEmail,
          maybe "" (\mobile -> "<p>Mobile: " <> mobile <> "</p>") customerMobile,
          "</div>",
          -- Bookings Table
          "<table class='bookings-table'>",
          "<thead>",
          "<tr>",
          "<th>Booking ID</th>",
          "<th>Date</th>",
          "<th>Ride Type</th>",
          "<th>From</th>",
          "<th>To</th>",
          "<th>Amount</th>",
          "</tr>",
          "</thead>",
          "<tbody>",
          T.concat $ map generateBookingRow bookings,
          "</tbody>",
          "</table>",
          -- Summary
          "<div class='summary'>",
          "<div class='summary-row'>",
          "<span class='summary-label'>Total Rides:</span>",
          "<span class='summary-value'>",
          T.pack $ show totalRides,
          "</span>",
          "</div>",
          "<div class='summary-row total'>",
          "<span class='summary-label'>Total Amount:</span>",
          "<span class='summary-value'>₹ ",
          formattedAmount,
          "</span>",
          "</div>",
          "</div>",
          -- Footer
          "<div class='footer'>",
          "<p>Thank you for choosing ",
          merchantName,
          "!</p>",
          "<p class='note'>This is a computer-generated invoice and does not require a signature.</p>",
          "</div>",
          "</div>",
          "</body>",
          "</html>"
        ]

-- | Generate table row for a booking
generateBookingRow :: DRB.Booking -> Text
generateBookingRow booking =
  let bookingId = getId booking.id
      createdAt = T.pack $ DT.formatTime DT.defaultTimeLocale "%Y-%m-%d %H:%M" booking.createdAt
      rideType = getRideType booking.bookingDetails
      fromLocation = fromMaybe "Unknown" booking.fromLocation.address.area
      toLocation = case getToLocation booking.bookingDetails of
        Just loc -> fromMaybe "Unknown" loc.address.area
        Nothing -> "N/A"
      amount = T.pack . show . getHighPrecMoney $ booking.estimatedTotalFare.amount
   in T.concat
        [ "<tr>",
          "<td>",
          T.take 8 bookingId,
          "...</td>",
          "<td>",
          createdAt,
          "</td>",
          "<td>",
          rideType,
          "</td>",
          "<td>",
          fromLocation,
          "</td>",
          "<td>",
          toLocation,
          "</td>",
          "<td>₹ ",
          amount,
          "</td>",
          "</tr>"
        ]

-- | Get ride type from booking details
getRideType :: DRB.BookingDetails -> Text
getRideType = \case
  DRB.OneWayDetails _ -> "One Way"
  DRB.RentalDetails _ -> "Rental"
  DRB.InterCityDetails _ -> "Inter City"
  DRB.AmbulanceDetails _ -> "Ambulance"
  DRB.DeliveryDetails _ -> "Delivery"
  DRB.MeterRideDetails _ -> "Meter Ride"
  DRB.DriverOfferDetails _ -> "Driver Offer"
  DRB.OneWaySpecialZoneDetails _ -> "Special Zone"

-- | Get to location from booking details
getToLocation :: DRB.BookingDetails -> Maybe DL.Location
getToLocation = \case
  DRB.OneWayDetails details -> Just details.toLocation
  DRB.RentalDetails details -> details.stopLocation
  DRB.InterCityDetails details -> Just details.toLocation
  DRB.AmbulanceDetails details -> Just details.toLocation
  DRB.DeliveryDetails details -> Just details.toLocation
  DRB.MeterRideDetails details -> details.toLocation
  DRB.DriverOfferDetails details -> Just details.toLocation
  DRB.OneWaySpecialZoneDetails details -> Just details.toLocation

-- | CSS styles for invoice
invoiceCSS :: Text
invoiceCSS =
  T.concat
    [ "* { margin: 0; padding: 0; box-sizing: border-box; }",
      "body { font-family: 'Arial', sans-serif; color: #333; background: #f5f5f5; padding: 20px; }",
      ".invoice-container { max-width: 900px; margin: 0 auto; background: white; padding: 40px; box-shadow: 0 0 10px rgba(0,0,0,0.1); }",
      ".header { display: flex; justify-content: space-between; margin-bottom: 30px; padding-bottom: 20px; border-bottom: 2px solid #FFC629; }",
      ".logo h1 { color: #FFC629; font-size: 28px; margin-bottom: 5px; }",
      ".logo p { color: #666; font-size: 14px; }",
      ".invoice-info { text-align: right; }",
      ".invoice-info h2 { color: #333; font-size: 24px; margin-bottom: 10px; }",
      ".invoice-info p { margin: 5px 0; color: #666; }",
      ".customer-info { margin-bottom: 30px; }",
      ".customer-info h3 { color: #FFC629; margin-bottom: 10px; font-size: 18px; }",
      ".customer-info p { margin: 5px 0; color: #666; }",
      ".bookings-table { width: 100%; border-collapse: collapse; margin-bottom: 30px; }",
      ".bookings-table th { background-color: #FFC629; color: white; padding: 12px; text-align: left; font-weight: bold; }",
      ".bookings-table td { padding: 12px; border-bottom: 1px solid #eee; }",
      ".bookings-table tr:hover { background-color: #f9f9f9; }",
      ".summary { border-top: 2px solid #FFC629; padding-top: 20px; }",
      ".summary-row { display: flex; justify-content: space-between; padding: 8px 0; }",
      ".summary-row.total { font-size: 20px; font-weight: bold; color: #FFC629; border-top: 1px solid #eee; margin-top: 10px; padding-top: 15px; }",
      ".footer { margin-top: 40px; text-align: center; padding-top: 20px; border-top: 1px solid #eee; }",
      ".footer p { color: #666; margin: 5px 0; }",
      ".footer .note { font-size: 12px; color: #999; margin-top: 10px; }"
    ]
