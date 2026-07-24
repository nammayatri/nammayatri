module Domain.Action.UI.FinanceInvoice
  ( getSubscriptionInvoices,
    getFinanceInvoicePdf,
  )
where

import qualified API.Types.UI.FinanceInvoice as API
import qualified Data.Time as DT
import "beckn-spec" Domain.Types.Invoice (InvoiceType (..), IssuedToType (..))
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person (Person)
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude (head, listToMaybe, showBaseUrl)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Domain.Types.Invoice as FinanceInvoice
import Lib.Finance.Invoice.PdfService
import qualified Lib.Finance.Invoice.RenderTemplate as FRT
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransaction as QIndirectTaxExtra
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QFinanceInvoiceExtra
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as HQPaymentTransaction
import qualified SharedLogic.RenderInvoiceFromTemplate as RIFT
import Storage.Beam.Payment ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.SubscriptionPurchase as QSubscriptionPurchase
import Tools.Error
import "beckn-services" Tools.InvoicePdf (generateFinanceInvoicePdf)

-- | List finance invoices for the authenticated driver/fleet owner.
getSubscriptionInvoices ::
  ( Maybe (Id Person),
    Id Merchant,
    Id MerchantOperatingCity
  ) ->
  Maybe UTCTime ->
  Maybe InvoiceType ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Flow API.FinanceInvoiceListRes
getSubscriptionInvoices (mbDriverId, _, _) mbFrom mbInvoiceType mbLimit mbOffset mbTo = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  _driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)

  now <- getCurrentTime
  let fromDate = mbFrom
      toDate = mbTo <|> Just now
      limit = min 20 . fromMaybe 10 $ mbLimit
      offset = fromMaybe 0 mbOffset

  let driverIdText = driverId.getId

  invoicesAll <- case mbInvoiceType of
    Just Ride ->
      QFinanceInvoiceExtra.findBySupplierAndType
        driverIdText
        (Just Ride)
        fromDate
        toDate
        (Just limit)
        (Just offset)
    _ ->
      QFinanceInvoiceExtra.findByIssuedToAndType
        driverIdText
        mbInvoiceType
        fromDate
        toDate
        (Just limit)
        (Just offset)

  -- Hide Voided/Cancelled (incl. 0-amount AggregatedCommission markers).
  let invoices = filter (\i -> i.status `notElem` [FinanceInvoice.Voided, FinanceInvoice.Cancelled]) invoicesAll

  items <- mapM buildInvoiceItem invoices

  pure $
    API.FinanceInvoiceListRes
      { invoices = items,
        totalItems = length items
      }
  where
    buildInvoiceItem :: FinanceInvoice.Invoice -> Flow API.FinanceInvoiceItem
    buildInvoiceItem invoice = do
      indirectTaxTxns <- QIndirectTaxExtra.findByInvoiceNumber (Just invoice.invoiceNumber)
      let mbTaxTxn = Kernel.Prelude.listToMaybe indirectTaxTxns

      mbPaymentMethod <- case invoice.entityReferenceId of
        Just orderId -> do
          txns <- HQPaymentTransaction.findAllByOrderId (Id orderId)
          pure $ Kernel.Prelude.listToMaybe txns >>= (.paymentMethod)
        Nothing -> pure Nothing

      mbTotalCredit <- case invoice.invoiceType of
        SubscriptionPurchase -> do
          mbSub <- QSubscriptionPurchase.findByFinanceInvoiceId invoice.id
          pure $ (.planRideCredit) <$> mbSub
        _ -> pure Nothing

      let taxAmount = invoice.totalAmount - invoice.subtotal

      pure $
        API.FinanceInvoiceItem
          { invoiceNumber = invoice.invoiceNumber,
            invoiceType = invoice.invoiceType,
            invoiceDate = invoice.issuedAt,
            taxAmount = taxAmount,
            taxableValue = invoice.subtotal,
            totalAmountPayable = invoice.totalAmount,
            gstRate = (.gstRate) <$> mbTaxTxn,
            cgstRate = mbTaxTxn >>= mkComponentRate (Just . (.cgstAmount)),
            sgstRate = mbTaxTxn >>= mkComponentRate (Just . (.sgstAmount)),
            igstRate = mbTaxTxn >>= mkComponentRate (Just . (.igstAmount)),
            sgstAmount = (.sgstAmount) <$> mbTaxTxn,
            cgstAmount = (.cgstAmount) <$> mbTaxTxn,
            igstAmount = (.igstAmount) <$> mbTaxTxn,
            totalGstAmount = (.totalGstAmount) <$> mbTaxTxn,
            paymentMethod = mbPaymentMethod,
            issuedToName = invoice.issuedToName,
            issuedToAddress = invoice.issuedToAddress,
            issuedByName = invoice.issuedByName,
            issuedByAddress = invoice.issuedByAddress,
            supplierAddress = invoice.supplierAddress,
            supplierName = invoice.supplierName,
            supplierGSTIN = invoice.supplierGSTIN,
            supplierTaxNo = invoice.supplierTaxNo,
            merchantGstin = invoice.merchantGstin,
            gstinOfParty = mbTaxTxn >>= (.gstinOfParty),
            sacCode = mbTaxTxn >>= (.sacCode),
            lineItems = Just invoice.lineItems,
            totalCredit = mbTotalCredit,
            taxRate = mbTaxTxn >>= (.taxRate),
            issuedToTaxNo = mbTaxTxn >>= (.issuedToTaxNo),
            issuedByTaxNo = mbTaxTxn >>= (.issuedByTaxNo)
          }

    mkComponentRate getAmount txn = do
      componentAmount <- getAmount txn
      guard (txn.taxableValue > 0)
      pure $ realToFrac (componentAmount / txn.taxableValue) * 100.0

-- | Generate a PDF for a single invoice.
getFinanceInvoicePdf ::
  ( Maybe (Id Person),
    Id Merchant,
    Id MerchantOperatingCity
  ) ->
  Maybe DateOrTime ->
  Maybe InvoiceType ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe DateOrTime ->
  Flow API.FinanceInvoicePdfResp
getFinanceInvoicePdf (mbDriverId, _, merchantOpCityId) mbFrom mbInvoiceType mbLimit mbOffset _mbReferenceId mbTo = do
  driverId <- mbDriverId & fromMaybeM (PersonNotFound "No person found")
  mbDriver <- QPerson.findById driverId
  mbTransporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing))

  let fromTime = toUTCTimeFrom <$> mbFrom
      toTime = toUTCTimeTo <$> mbTo

  invoicesAll <-
    QFinanceInvoiceExtra.findByMerchantOpCityIdAndDateRange
      merchantOpCityId.getId
      fromTime
      toTime
      mbInvoiceType
      Nothing
      (Just driverId.getId)
      Nothing
      Nothing
      []
      []
      (mbLimit <|> Just 10)
      (mbOffset <|> Just 0)

  -- Hide Voided/Cancelled (incl. 0-amount AggregatedCommission markers).
  let invoices = filter (\i -> i.status `notElem` [FinanceInvoice.Voided, FinanceInvoice.Cancelled]) invoicesAll

  when (null invoices) $
    throwError $ InvalidRequest "No invoices found for the given criteria"

  let inv = head invoices
      items = parseLineItems inv.lineItems

  taxTxns <- QIndirectTaxExtra.findByInvoiceNumber (Just inv.invoiceNumber)
  let mbTaxTxn = Kernel.Prelude.listToMaybe taxTxns

  (mbPayType, mbBrand, mbLast4) <- case inv.entityReferenceId of
    Just orderId -> do
      txns <- HQPaymentTransaction.findAllByOrderId (Id orderId)
      let mbTxn = Kernel.Prelude.listToMaybe txns
      pure (mbTxn >>= (.paymentMethodType), mbTxn >>= (.cardBrand), mbTxn >>= (.cardLastFourDigits))
    Nothing -> pure (Nothing, Nothing, Nothing)

  -- Live-fetch AggregatedCommission party metadata (Y-tunnus + merchant VAT)
  -- since these aren't persisted on the Invoice row.
  (mbRecipientBid, mbSellerBid, mbSellerVat) <- case inv.invoiceType of
    AggregatedCommission -> do
      mbRecipientBid' <- case inv.issuedToType of
        FLEET_OWNER -> do
          mbFleet <- QFOI.findByPrimaryKey (Id inv.issuedToId)
          pure $ mbFleet >>= (.businessLicenseNumberDec)
        _ -> pure Nothing
      mbMerchant <- CQM.findById (Id inv.merchantId)
      pure (mbRecipientBid', mbMerchant >>= (.businessId), mbMerchant >>= (.vatNumber))
    _ -> pure (Nothing, Nothing, Nothing)

  let lang = fromMaybe ENGLISH (mbDriver >>= (.language))
      tz = maybe DT.utc (\tc -> DT.minutesToTimeZone (fromIntegral tc.timeDiffFromUtc `div` 60)) mbTransporterConfig
      ctx =
        FRT.buildInvoiceContext
          FRT.BuildInvoiceContextInput
            { language = lang,
              logoUrl = mbTransporterConfig >>= (.invoiceConfig) >>= (.logoUrl) <&> showBaseUrl,
              sellerTradeName = mbTransporterConfig >>= (.invoiceConfig) >>= (.invoiceSellerTradeName),
              appName = mbTransporterConfig >>= (.invoiceConfig) >>= (.invoiceAppName),
              invoice = inv,
              lineItems = items,
              mbTaxTxn = mbTaxTxn,
              mbPaymentMode = mbPayType,
              mbCardBrand = mbBrand,
              mbCardLastFour = mbLast4,
              mbRecipientBusinessId = mbRecipientBid,
              mbSellerBusinessId = mbSellerBid,
              mbSellerVatNumber = mbSellerVat
            }
      mbInvType = case inv.invoiceType of
        AggregatedCommission -> Just AggregatedCommission
        _ -> Nothing
  html <- RIFT.renderHtml (Id inv.merchantOperatingCityId) mbInvType lang tz ctx
  pdfBase64 <- generateFinanceInvoicePdf inv.invoiceNumber html

  pure $
    API.FinanceInvoicePdfResp
      { pdfBase64 = pdfBase64,
        invoiceNumber = inv.invoiceNumber
      }
