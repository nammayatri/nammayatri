{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.PartnerBookingStatement (postCorporateBookingStatement, postCorporateInvoiceData) where

import qualified API.Types.UI.PartnerBookingStatement
import qualified API.Types.UI.PartnerBookingStatement as PBSAPI
import API.Types.UI.PartnerBookingStatementExtra ()
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Data.Time.Calendar (Day, addDays)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
-- import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Booking as DBooking
import qualified Domain.Types.Booking.API as DBAPI
import Domain.Types.Location (Location (..), LocationAPIEntity (..))
import Domain.Types.LocationAddress (LocationAddress (..))
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RideStatus as DRide
import qualified Environment
-- import qualified Domain.Types.Ride as DRide
-- import qualified Domain.Types.ServiceTierType as DSTT
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, getDbHash)
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (MissingHeader, throwError)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Auth

-- | Corporate Invoice Data API
postCorporateInvoiceData :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.PartnerBookingStatement.InvoiceDataReq -> Environment.Flow API.Types.UI.PartnerBookingStatement.InvoiceDataRes)
postCorporateInvoiceData mbApiKey req = do
  apiKey <- mbApiKey & fromMaybeM (MissingHeader "X-Partner-API-Key")
  corporatePartnerApiToken <- asks (.corporatePartnerApiToken)
  unless (apiKey == corporatePartnerApiToken) $
    throwError (InvalidToken "Invalid corporate partner API token")

  -- Resolve person
  mbPerson <-
    case req.partnerCustomerId of
      Just partnerCustomerId -> B.runInReplica $ QPerson.findById (Id partnerCustomerId)
      Nothing -> do
        merchant <- findMerchantByShortId (ShortId "NAMMA_YATRI")
        case req.mobileNumber of
          Just mobileNum -> do
            let (countryCode, phoneNumber) = parseMobileNumber mobileNum
            mobileNumberHash <- getDbHash phoneNumber
            B.runInReplica $ QPerson.findByMobileNumberAndMerchantId countryCode mobileNumberHash merchant.id
          Nothing -> pure Nothing

  person <- mbPerson & fromMaybeM (InvalidRequest "Person not found")

  -- Fetch booking and build API entity
  booking <- QBooking.findById req.bookingId >>= fromMaybeM (InvalidRequest "Booking not found")
  bookingAPIEntity <- DBAPI.buildBookingAPIEntity booking person.id

  let mbRide = find (\ride -> ride.status == DRide.COMPLETED) bookingAPIEntity.rideList
      paymentCurrency = show bookingAPIEntity.estimatedTotalFareWithCurrency.currency
  case mbRide of
    Nothing ->
      pure
        PBSAPI.InvoiceDataRes
          { responseCode = "400",
            outcomeCode = "400",
            responseMessage = "No completed ride found for the booking",
            partnerCustomerId = person.id.getId,
            emailId = fromMaybe "" req.emailId,
            mobileNumber = fromMaybe "" req.mobileNumber,
            bookingId = booking.id,
            bookingDatetime = bookingAPIEntity.createdAt,
            bookingStatus = bookingAPIEntity.status,
            item = [],
            person = [],
            unitPricing = [],
            gst = [],
            paymentMode = [],
            billing =
              PBSAPI.InvoiceBilling
                { itemAmount = 0,
                  addonAmount = 0,
                  discountAmount = 0,
                  paymentAmount = 0.0,
                  paymentAmountCurrencyName = Just paymentCurrency
                },
            invoice =
              PBSAPI.InvoiceInvoice
                { invoiceDatetime = Just bookingAPIEntity.createdAt,
                  invoiceAmount = 0,
                  invoiceAmountCurrencyName = Just paymentCurrency
                }
          }
    Just ride -> do
      let provider = "Namma Yatri"
          paymentAmount = fromMaybe (toHighPrecMoney bookingAPIEntity.estimatedTotalFare) (toHighPrecMoney <$> ride.computedPrice)
          fromLocation = bookingAPIEntity.fromLocation
          toLocation = getToLocation bookingAPIEntity
          bookingDatetime = bookingAPIEntity.createdAt
          serviceStartDatetime = ride.rideStartTime <|> Just bookingDatetime
          serviceEndDatetime = ride.rideEndTime <|> serviceStartDatetime

          itemEntry =
            PBSAPI.InvoiceItem
              { itemId = ride.shortRideId.getShortId,
                itemName = "Ride",
                bookingType = bookingAPIEntity.vehicleCategory,
                bookingClass = bookingAPIEntity.vehicleServiceTierType,
                provider = provider,
                providerCode = ride.vehicleNumber,
                providerPriceCategory = show ride.vehicleVariant,
                serviceStartAddress = formatAddress fromLocation,
                serviceStartPincode = fromLocation.areaCode,
                serviceStartDatetime = serviceStartDatetime,
                serviceEndAddress = fromMaybe "" (formatAddress <$> toLocation),
                serviceEndPincode = toLocation >>= (.areaCode),
                serviceEndDatetime = serviceEndDatetime
              }

          personEntry =
            PBSAPI.InvoicePerson
              { personId = person.id.getId,
                gender = show person.gender,
                firstName = fromMaybe "" person.firstName,
                middleName = fromMaybe "" person.middleName,
                lastName = fromMaybe "" person.lastName
              }

          unitPricingEntry =
            PBSAPI.InvoiceUnitPricing
              { itemId = "ride",
                personId = person.id.getId,
                itemAmount = paymentAmount,
                platformFee = 0,
                discountAmount = 0,
                priceMultipler = 1
              }

          paymentModeEntry =
            PBSAPI.InvoicePaymentMode
              { paymentMode = "CASH/UPI",
                amount = paymentAmount
              }

          billingEntry =
            PBSAPI.InvoiceBilling
              { itemAmount = paymentAmount,
                addonAmount = 0,
                discountAmount = 0,
                paymentAmount = paymentAmount,
                paymentAmountCurrencyName = Just paymentCurrency
              }

          invoiceEntry =
            PBSAPI.InvoiceInvoice
              { invoiceDatetime = serviceEndDatetime,
                invoiceAmount = paymentAmount,
                invoiceAmountCurrencyName = Just paymentCurrency
              }

      email <- mapM decrypt person.email

      pure
        PBSAPI.InvoiceDataRes
          { responseCode = "200",
            outcomeCode = "200",
            responseMessage = "Success",
            partnerCustomerId = person.id.getId,
            emailId = fromMaybe "" (req.emailId <|> email),
            mobileNumber = fromMaybe "" req.mobileNumber,
            bookingId = booking.id,
            bookingDatetime = bookingDatetime,
            bookingStatus = bookingAPIEntity.status,
            item = [itemEntry],
            person = [personEntry],
            unitPricing = [unitPricingEntry],
            gst = [],
            paymentMode = [paymentModeEntry],
            billing = billingEntry,
            invoice = invoiceEntry
          }

postCorporateBookingStatement :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.PartnerBookingStatement.BookingStatementReq -> Environment.Flow API.Types.UI.PartnerBookingStatement.BookingStatementRes)
postCorporateBookingStatement mbApiKey req = do
  -- Validate partner API token
  apiKey <- mbApiKey & fromMaybeM (MissingHeader "X-Partner-API-Key")
  corporatePartnerApiToken <- asks (.corporatePartnerApiToken)
  unless (apiKey == corporatePartnerApiToken) $
    throwError (InvalidToken "Invalid corporate partner API token")

  -- Process the request
  let mobileNumber = req.mobileNumber
      fromDate = req.fromDate
      toDate = req.toDate
      _pageSize = fromMaybe 10 req.count
      _page = fromMaybe 1 req.page

  -- Parse mobile number (expected format: 91XXXXXXXXXX)
  let (countryCode, phoneNumber) = parseMobileNumber mobileNumber

  -- Find person by mobile number
  mbPerson <- case req.partnerCustomerId of
    Just partnerCustomerId -> do
      B.runInReplica $ QPerson.findById (Id partnerCustomerId)
    Nothing -> do
      merchant <- findMerchantByShortId (ShortId "NAMMA_YATRI")
      mobileNumberHash <- getDbHash phoneNumber
      B.runInReplica $ QPerson.findByMobileNumberAndMerchantId countryCode mobileNumberHash merchant.id

  case mbPerson of
    Nothing -> do
      -- Return empty response if person not found
      return $
        PBSAPI.BookingStatementRes
          { responseCode = "400",
            outcomeCode = "400",
            responseMessage = "Person not registered with the NammaYatri",
            mobileNumber = mobileNumber,
            emailId = fromMaybe "" req.emailId,
            partnerCustomerId = req.partnerCustomerId,
            item = []
          }
    Just person -> do
      -- Convert Day to UTCTime for query
      let startTime = dayToUTCTime fromDate
          endTime = dayToEndOfDay toDate

      -- Find completed bookings in date range
      bookings <- B.runInReplica $ QBooking.findBookingsForInvoice person.id startTime endTime Nothing Nothing
      -- For each booking, get the ride details and build the response
      allBookingAPIEntities <- mapM (`DBAPI.buildBookingAPIEntity` person.id) bookings

      -- Filter: Keep only bookings with exactly 1 completed ride
      let validBookingAPIEntities = filter hasExactlyOneRide allBookingAPIEntities

      items <- catMaybes <$> mapM (buildBookingItem person) validBookingAPIEntities
      email <- mapM decrypt person.email

      return $
        PBSAPI.BookingStatementRes
          { responseCode = "200",
            outcomeCode = "200",
            responseMessage = "Success",
            mobileNumber = mobileNumber,
            emailId = fromMaybe "" (req.emailId <|> email),
            partnerCustomerId = Just person.id.getId,
            item = items
          }

-- | Parse mobile number from format 91XXXXXXXXXX to ("+91", "XXXXXXXXXX")
parseMobileNumber :: Text -> (Text, Text)
parseMobileNumber number =
  if T.length number > 2 && T.take 2 number == "91"
    then ("+91", T.drop 2 number)
    else ("+91", number)

-- | Convert Day to UTCTime at start of day (00:00:00)
dayToUTCTime :: Day -> UTCTime
dayToUTCTime day = UTCTime day (secondsToDiffTime 0)

-- | Convert Day to UTCTime at end of day (23:59:59)
dayToEndOfDay :: Day -> UTCTime
dayToEndOfDay day = UTCTime (addDays 1 day) (secondsToDiffTime 0)

-- | Build a BookingStatementItem from a Booking and its associated Ride
buildBookingItem ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  DP.Person ->
  DBAPI.BookingAPIEntity ->
  m (Maybe PBSAPI.BookingStatementItem)
buildBookingItem _person bookingAPIEntity = do
  let mbRide = find (\ride -> ride.status == DRide.COMPLETED) bookingAPIEntity.rideList
  case mbRide of
    Nothing -> return Nothing
    Just ride -> do
      let provider = "Namma Yatri"
          providerCode = ride.vehicleNumber
          -- Get payment amount
          paymentAmount = fromMaybe (toHighPrecMoney bookingAPIEntity.estimatedTotalFare) (toHighPrecMoney <$> ride.computedPrice)

          -- Get addresses
          fromLocation = bookingAPIEntity.fromLocation
          toLocation = getToLocation bookingAPIEntity

          bookingDatetime = bookingAPIEntity.createdAt
          serviceStartDatetime = fromMaybe bookingDatetime ride.rideStartTime
          serviceEndDatetime = fromMaybe serviceStartDatetime ride.rideEndTime

      return $
        Just
          PBSAPI.BookingStatementItem
            { bookingId = bookingAPIEntity.id,
              bookingDatetime = bookingDatetime,
              bookingType = bookingAPIEntity.vehicleCategory,
              provider = provider,
              providerCode = providerCode,
              paymentAmount = paymentAmount,
              paymentAmountCurrencyCode = show bookingAPIEntity.estimatedTotalFareWithCurrency.currency,
              serviceStartAddress = (formatAddress fromLocation),
              serviceStartPincode = fromLocation.areaCode,
              serviceStartDatetime = serviceStartDatetime,
              serviceEndAddress = fromMaybe "" (formatAddress <$> toLocation),
              serviceEndPincode = (toLocation >>= (.areaCode)),
              serviceEndDatetime = serviceEndDatetime
            }

-- | Get destination location from booking details
getToLocation :: DBAPI.BookingAPIEntity -> Maybe LocationAPIEntity
getToLocation booking =
  case booking.bookingDetails of
    DBAPI.OneWayAPIDetails details -> Just details.toLocation
    DBAPI.DriverOfferAPIDetails details -> Just details.toLocation
    DBAPI.OneWaySpecialZoneAPIDetails details -> Just details.toLocation
    DBAPI.InterCityAPIDetails details -> Just details.toLocation
    DBAPI.AmbulanceAPIDetails details -> Just details.toLocation
    DBAPI.DeliveryAPIDetails details -> Just details.toLocation
    DBAPI.RentalAPIDetails details -> details.stopLocation
    DBAPI.MeterRideAPIDetails details -> details.toLocation

-- | Format address from LocationAddress
formatAddress :: LocationAPIEntity -> Text
formatAddress address =
  T.intercalate ", " $
    filter (not . T.null) $
      catMaybes
        [ address.door,
          address.building,
          address.street,
          address.area,
          address.city,
          address.areaCode,
          address.state,
          address.country
        ]

-- -- | Format UTCTime to ISO 8601 format with timezone offset
-- formatISO8601 :: UTCTime -> Text
-- formatISO8601 time = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S+05:30" time

-- -- | Show HighPrecMoney as text with 2 decimal places
-- showHighPrecMoney :: HighPrecMoney -> Text
-- showHighPrecMoney amount = T.pack $ show (realToFrac amount :: Double)

-- | Check if booking has at least one completed ride
hasExactlyOneRide :: DBAPI.BookingAPIEntity -> Bool
hasExactlyOneRide booking =
  let completedRides = filter (\ride -> ride.status == DRide.COMPLETED) booking.rideList
   in length completedRides == 1
