module Domain.Action.Dashboard.Invoice
  ( getInvoiceInvoice,
    getInvoiceFinanceInvoicePdf,
  )
where

import qualified "dashboard-helper-api" API.Types.RiderPlatform.Management.Invoice as Common
import qualified Data.Time as DT
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance.Domain.Types.Invoice (InvoiceType)
import Lib.Finance.Invoice.PdfService
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra as QIndirectTaxExtra
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QInvoiceExtra
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as HQPaymentTransaction
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Clickhouse.Booking as CHB
import qualified Storage.Clickhouse.FareBreakup as CHFB
import qualified Storage.Clickhouse.Location as CHL
import qualified Storage.Clickhouse.Ride as CHR
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.Person as QP
import Tools.Error
import "beckn-services" Tools.InvoicePdf (generateFinanceInvoicePdf)

getInvoiceFinanceInvoicePdf ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe DateOrTime ->
  Maybe InvoiceType ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe DateOrTime ->
  Flow Common.FinanceInvoicePdfResp
getInvoiceFinanceInvoicePdf merchantShortId opCity mbFrom mbInvoiceType mbLimit mbOffset mbReferenceId mbTo = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantId: " <> merchant.id.getId <> ", city: " <> show opCity)
  mbRiderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOpCity.id.getId})

  let fromTime = toUTCTimeFrom <$> mbFrom
      toTime = toUTCTimeTo <$> mbTo

  invoices <-
    QInvoiceExtra.findByMerchantOpCityIdAndDateRange
      merchantOpCity.id.getId
      fromTime
      toTime
      mbInvoiceType
      Nothing
      mbReferenceId
      Nothing
      (mbLimit <|> Just 10)
      (mbOffset <|> Just 0)

  when (Kernel.Prelude.null invoices) $
    throwError $ InvalidRequest "No invoices found for the given criteria"

  let locale = countryToLocale merchantOpCity.country
      tz = maybe DT.utc (\rc -> DT.minutesToTimeZone (fromIntegral rc.timeDiffFromUtc `div` 60)) mbRiderConfig
      cfg = InvoicePdfConfig {locale, timezone = tz, logoUrl = mbRiderConfig >>= (.invoiceConfig) >>= (.logoUrl)}

  pdfDatas <- forM invoices $ \inv -> do
    let items = parseLineItems inv.lineItems
    taxTxns <- QIndirectTaxExtra.findByInvoiceNumber inv.invoiceNumber
    let mbTaxTxn = listToMaybe taxTxns
    (mbPayType, mbBrand, mbLast4) <- case inv.paymentOrderId of
      Just orderId -> do
        txns <- HQPaymentTransaction.findAllByOrderId (Id orderId)
        let mbTxn = listToMaybe txns
        pure (mbTxn >>= (.paymentMethodType), mbTxn >>= (.cardBrand), mbTxn >>= (.cardLastFourDigits))
      Nothing -> pure (Nothing, Nothing, Nothing)
    pure $ buildInvoicePdfData inv items mbTaxTxn mbPayType mbBrand mbLast4

  let lastInv = last invoices
      html = case pdfDatas of
        [single] -> renderInvoiceHtml cfg single
        batch -> renderBatchInvoiceHtml cfg batch

  pdfBase64 <- generateFinanceInvoicePdf lastInv.invoiceNumber html

  pure $
    Common.FinanceInvoicePdfResp
      { pdfBase64 = pdfBase64,
        invoiceNumber = lastInv.invoiceNumber
      }
  where
    countryToLocale Context.Finland = FI
    countryToLocale Context.Netherlands = NL
    countryToLocale _ = EN


getInvoiceInvoice :: ShortId DM.Merchant -> Context.City -> UTCTime -> Text -> UTCTime -> Flow [Common.InvoiceRes]
getInvoiceInvoice merchantShortId _ from phoneNumber to = do
  merchant <- findMerchantByShortId merchantShortId
  phoneNumberDBHash <- getDbHash phoneNumber
  person <- B.runInReplica $ QP.findByMobileNumberAndMerchantId "+91" phoneNumberDBHash merchant.id >>= fromMaybeM (PersonWithPhoneNotFound phoneNumber)
  bookings <- CHB.findAllCompletedRiderBookingsByMerchantInRange merchant.id person.id from to
  invoices <- mapM makeInvoiceResponse bookings
  return $ catMaybes invoices
  where
    makeInvoiceResponse booking = do
      mbRide <- CHR.findRideByBookingId booking.id booking.createdAt
      case mbRide of
        Just ride -> do
          let breakupItems =
                [ ("BASE_FARE", "Base Fare"),
                  ("CUSTOMER_SELECTED_FARE", "Customer Selected Fare"),
                  ("DEAD_KILOMETER_FARE", "Dead Kilometer Fare"),
                  ("DISTANCE_FARE", "Distance Fare"),
                  ("DRIVER_SELECTED_FARE", "Driver Selected Fare"),
                  ("EXTRA_TIME_FARE", "Extra Time Fare"),
                  ("FIXED_GOVERNMENT_RATE", "Fixed Government Fare"),
                  ("NIGHT_SHIFT_CHARGE", "Night Shift Charge"),
                  ("PLATFORM_FEE", "Platform Fee"),
                  ("CGST", "CGST"),
                  ("SGST", "SGST"),
                  ("SERVICE_CHARGE", "Service Charge"),
                  ("TIME_BASED_FARE", "Time Based Fare"),
                  ("DIST_BASED_FARE", "Distance Based Fare"),
                  ("EXTRA_DISTANCE_FARE", "Extra Distance Fare"),
                  ("WAITING_OR_PICKUP_CHARGES", "Wating Charge"),
                  ("PARKING_CHARGE", "Parking Charge"),
                  ("RIDE_STOP_CHARGES", "Ride Stop Charges"),
                  ("PER_STOP_CHARGES", "Per Stop Charges"),
                  ("LUGGAGE_CHARGE", "Luggage Charge"),
                  ("DRIVER_ALLOWANCE", "Driver Allowance"),
                  ("AIRPORT_CONVENIENCE_FEE", "Airport Convenience Fee"),
                  ("RETURN_FEE", "Return Fee"),
                  ("BOOTH_CHARGE", "Booth Charge")
                ]
          fareBreakups <- mapM (getFareBreakup booking) breakupItems
          mbSource <- case booking.fromLocationId of
            Just fromLocId -> CHL.findLocationById fromLocId booking.createdAt
            Nothing -> return Nothing
          mbDestination <- case booking.toLocationId of
            Just toLocId -> CHL.findLocationById toLocId booking.createdAt
            Nothing -> return Nothing
          return $
            Just $
              Common.InvoiceRes
                { date = booking.createdAt,
                  destination = maybe notAvailableText (\destination -> fromMaybe notAvailableText destination.fullAddress) mbDestination,
                  driverName = fromMaybe notAvailableText ride.driverName,
                  faresList = catMaybes fareBreakups,
                  rideEndTime = fromMaybe ride.updatedAt ride.rideEndTime,
                  rideStartTime = fromMaybe ride.createdAt ride.rideStartTime,
                  shortRideId = ride.shortId.getShortId,
                  source = maybe notAvailableText (\source -> fromMaybe notAvailableText source.fullAddress) mbSource,
                  totalAmount = maybe notAvailableText show ride.totalFare,
                  vehicleNumber = fromMaybe notAvailableText ride.vehicleNumber,
                  chargeableDistance = ride.chargeableDistance,
                  chargeableDistanceWithUnit = convertHighPrecMetersToDistance Meter <$> ride.chargeableDistance -- FIXME use proper unit
                }
        Nothing -> return Nothing
    getFareBreakup booking (tag, title) = do
      fareBreakup <- CHFB.findFareBreakupByBookingIdAndDescription booking.id tag booking.createdAt
      case fareBreakup of
        Just breakup -> return . Just $ Common.FareBreakup {price = maybe notAvailableText show breakup.amount, title}
        Nothing -> return Nothing
    notAvailableText = "N/A"
