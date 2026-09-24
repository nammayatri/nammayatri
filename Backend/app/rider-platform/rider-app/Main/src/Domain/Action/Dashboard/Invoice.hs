module Domain.Action.Dashboard.Invoice
  ( getInvoiceInvoice,
    getInvoiceFinanceList,
    getInvoiceFinancePdf,
  )
where

import qualified "this" API.Types.RiderPlatform.Management.Invoice as Common
import qualified BecknV2.OnDemand.Enums as Enums
import qualified Dashboard.Common
import qualified Data.Text as T
import Data.Time (UTCTime (..), addGregorianMonthsClip, fromGregorian, toGregorian)
import qualified Domain.Action.UI.FinanceInvoice as UIFinanceInvoice
import qualified "beckn-spec" Domain.Types.Invoice as DInvoice
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransaction as QIndirectTax
import qualified Lib.Finance.Storage.Queries.Invoice as QFinanceInvoice
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QFinanceInvoiceExtra
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as HQPaymentTransaction
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Clickhouse.Booking as CHB
import qualified Storage.Clickhouse.FareBreakup as CHFB
import qualified Storage.Clickhouse.Location as CHL
import qualified Storage.Clickhouse.Ride as CHR
import qualified Storage.Queries.Person as QP
import Tools.Error

getInvoiceInvoice :: ShortId DM.Merchant -> Context.City -> UTCTime -> Text -> UTCTime -> Flow [Common.InvoiceRes]
getInvoiceInvoice merchantShortId _ from phoneNumber to = do
  merchant <- findMerchantByShortId merchantShortId
  phoneNumberDBHash <- getDbHash phoneNumber
  person <- B.runInReplica $ QP.findByMobileNumberAndMerchantId "+91" phoneNumberDBHash merchant.id >>= fromMaybeM (PersonWithPhoneNotFound phoneNumber)
  let batchRanges = makeMonthlyBatchRanges from to
  bookings <- concat <$> mapM (\(bFrom, bTo) -> CHB.findAllCompletedRiderBookingsByMerchantInRange merchant.id person.id bFrom bTo) batchRanges
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
                  ("BOOTH_CHARGE", "Booth Charge"),
                  ("SCHEDULING_CHARGE", "Scheduling Charge")
                ]
          fareBreakups <- mapM (getFareBreakup booking) breakupItems
          gateFeeBreakups <- getGateFeeBreakups booking
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
                  destination = maybe notAvailableText buildAddress mbDestination,
                  driverName = fromMaybe notAvailableText ride.driverName,
                  faresList = catMaybes fareBreakups <> gateFeeBreakups,
                  rideEndTime = fromMaybe ride.updatedAt ride.rideEndTime,
                  rideStartTime = fromMaybe ride.createdAt ride.rideStartTime,
                  shortRideId = ride.shortId.getShortId,
                  source = maybe notAvailableText buildAddress mbSource,
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
    -- A gate fee item's title carries an operator-configured name, so it cannot be
    -- in the fixed list above. Pick them out of the stored breakups by their
    -- GATE_FEE: prefix and show each under its configured name. Matching on the
    -- prefix rather than "anything unrecognised" keeps the internal summary tags
    -- (RIDE_FARE_*, PAYMENT_CHARGE_*, ...) off the invoice.
    getGateFeeBreakups booking = do
      breakups <- CHFB.findFareBreakupsByBookingId booking.id booking.createdAt
      return
        [ Common.FareBreakup {price = maybe notAvailableText show breakup.amount, title}
          | breakup <- breakups,
            Just title <- [Enums.gateFeeBreakupItemName breakup.description]
        ]
    buildAddress loc =
      case loc.ward of
        Just w -> w
        Nothing ->
          let parts = catMaybes [loc.area, loc.street, loc.building, loc.city]
           in if Kernel.Prelude.null parts then notAvailableText else T.intercalate ", " parts
    notAvailableText = "N/A"

makeMonthlyBatchRanges :: UTCTime -> UTCTime -> [(UTCTime, UTCTime)]
makeMonthlyBatchRanges start end
  | start >= end = []
  | otherwise =
    let (y, m, _) = toGregorian (utctDay start)
        firstOfNextMonth = UTCTime (addGregorianMonthsClip 1 (fromGregorian y m 1)) 0
        next = min end firstOfNextMonth
     in (start, next) : makeMonthlyBatchRanges next end

-- | Rider-side (BAP) finance invoice register. Always scoped to RIDER invoices
-- of the given city; invoiceId / invoiceNumber are exact lookups that bypass
-- the other filters (mirrors the BPP FinanceManagement invoice list).
getInvoiceFinanceList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe Text ->
  Maybe DInvoice.InvoiceType ->
  Maybe Int ->
  Maybe Int ->
  Maybe FInvoice.InvoiceStatus ->
  Maybe UTCTime ->
  Flow Common.FinanceInvoiceListRes
getInvoiceFinanceList merchantShortId opCity mbFrom mbInvoiceId mbInvoiceNumber mbInvoiceType mbLimit mbOffset mbStatus mbTo = do
  merchantOpCityId <- getMerchantOpCityId merchantShortId opCity
  let limit = min 100 . max 0 $ fromMaybe 20 mbLimit
      offset = max 0 $ fromMaybe 0 mbOffset
      inCity inv = inv.merchantOperatingCityId == merchantOpCityId && inv.issuedToType == DInvoice.RIDER
  invoicesAll <- case (mbInvoiceId, mbInvoiceNumber) of
    (Just invoiceId, _) -> filter inCity . maybeToList <$> QFinanceInvoice.findById (Id invoiceId)
    (Nothing, Just invoiceNumber) -> filter inCity . maybeToList <$> QFinanceInvoice.findByNumber invoiceNumber
    (Nothing, Nothing) ->
      QFinanceInvoiceExtra.findByMerchantOpCityIdAndDateRange
        merchantOpCityId
        mbFrom
        mbTo
        mbInvoiceType
        mbStatus
        Nothing
        Nothing
        (Just DInvoice.RIDER)
        []
        (if isJust mbStatus then [] else [FInvoice.Draft, FInvoice.Issued, FInvoice.Paid])
        (Just limit)
        (Just offset)
  -- Default (no status filter): hide Voided/Cancelled, same as the BPP register.
  let invoices = case mbStatus of
        Just _ -> invoicesAll
        Nothing -> filter (\i -> i.status `Kernel.Prelude.notElem` [FInvoice.Voided, FInvoice.Cancelled]) invoicesAll
  items <- mapM buildFinanceInvoiceItem invoices
  let totalItems = Kernel.Prelude.length items
  pure $
    Common.FinanceInvoiceListRes
      { totalItems,
        summary = Dashboard.Common.Summary {totalCount = totalItems, count = totalItems},
        invoices = items
      }
  where
    buildFinanceInvoiceItem :: FInvoice.Invoice -> Flow Common.FinanceInvoiceListItem
    buildFinanceInvoiceItem invoice = do
      mbTaxTxn <- listToMaybe <$> QIndirectTax.findByInvoiceNumber (Just invoice.invoiceNumber)
      mbPaymentMethod <- case invoice.entityReferenceId of
        Just orderId -> do
          txns <- HQPaymentTransaction.findAllByOrderId (Id orderId)
          pure $ listToMaybe txns >>= (.paymentMethod)
        Nothing -> pure Nothing
      pure $
        Common.FinanceInvoiceListItem
          { invoiceId = invoice.id.getId,
            invoiceNumber = invoice.invoiceNumber,
            invoiceType = invoice.invoiceType,
            invoiceDate = invoice.issuedAt,
            invoiceStatus = invoice.status,
            counterpartyType = show invoice.issuedToType,
            counterpartyId = invoice.issuedToId,
            taxableValue = (.taxableValue) <$> mbTaxTxn,
            gstRate = (.gstRate) <$> mbTaxTxn,
            gstAmount = (.totalGstAmount) <$> mbTaxTxn,
            cgstAmount = (.cgstAmount) <$> mbTaxTxn,
            sgstAmount = (.sgstAmount) <$> mbTaxTxn,
            igstAmount = (.igstAmount) <$> mbTaxTxn,
            totalInvoiceValue = invoice.totalAmount,
            irn = invoice.irn,
            qrCode = invoice.signedQRCode,
            rideId = invoice.referenceId,
            supplierName = invoice.supplierName,
            supplierAddress = invoice.supplierAddress,
            supplierGstin = invoice.supplierGSTIN,
            supplierTaxNo = invoice.supplierTaxNo,
            supplierId = invoice.supplierId,
            merchantGstin = invoice.merchantGstin,
            issuedToName = invoice.issuedToName,
            issuedToAddress = invoice.issuedToAddress,
            issuedByName = invoice.issuedByName,
            issuedByAddress = invoice.issuedByAddress,
            gstinOfParty = mbTaxTxn >>= (.gstinOfParty),
            sacCode = mbTaxTxn >>= (.sacCode),
            paymentMethod = mbPaymentMethod,
            taxableValueOfServiceSupplied = Just invoice.subtotal,
            lineItems = invoice.lineItems,
            generatedAt = invoice.createdAt,
            taxRate = mbTaxTxn >>= (.taxRate),
            issuedToTaxNo = mbTaxTxn >>= (.issuedToTaxNo),
            issuedByTaxNo = mbTaxTxn >>= (.issuedByTaxNo)
          }

getInvoiceFinancePdf :: ShortId DM.Merchant -> Context.City -> Text -> Flow Common.FinanceInvoicePdfRes
getInvoiceFinancePdf merchantShortId opCity invoiceId = do
  merchantOpCityId <- getMerchantOpCityId merchantShortId opCity
  invoice <- QFinanceInvoice.findById (Id invoiceId) >>= fromMaybeM (InvalidRequest $ "Invoice not found: " <> invoiceId)
  unless (invoice.merchantOperatingCityId == merchantOpCityId) $
    throwError $ InvalidRequest "Invoice does not belong to this city"
  (pdfBase64, invoiceNumber) <- UIFinanceInvoice.renderFinanceInvoicePdf merchantOpCityId ENGLISH [invoice]
  pure $ Common.FinanceInvoicePdfRes {pdfBase64, invoiceNumber}

getMerchantOpCityId :: ShortId DM.Merchant -> Context.City -> Flow Text
getMerchantOpCityId merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  pure merchantOpCity.id.getId
