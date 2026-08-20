{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Render a finance invoice to a PDF and store it in S3.
--
--   'renderInvoicePdfBase64' builds the invoice HTML context (same inputs as the
--   on-demand @/finance/invoice/pdf@ endpoint) and returns a base64 PDF.
--
--   'generateAndStoreInvoicePdf' renders the PDF and uploads it to S3, stamping
--   the object path onto the invoice row ('pdfS3Path'). It is idempotent (skips
--   if already stored) and never throws — a failure just leaves 'pdfS3Path' NULL,
--   and the read path falls back to on-demand rendering.
module SharedLogic.Finance.InvoiceDocument
  ( renderInvoicePdfBase64,
    storeInvoicePdf,
    generateAndStoreInvoicePdf,
    getInvoiceDocumentUrl,
    mkInvoiceQrDataUri,
    mkInvoicePdfUrl,
  )
where

import qualified AWS.S3 as S3
import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Data.Time as DT
import qualified Domain.Types.Booking as DRB
import "beckn-spec" Domain.Types.Invoice (InvoiceType (..), IssuedToType (..))
import qualified Domain.Types.Person as DP
import Environment (Flow)
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Domain.Types.Invoice as FinanceInvoice
import Lib.Finance.Invoice.PdfService (parseLineItems)
import qualified Lib.Finance.Invoice.RenderTemplate as FRT
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransaction as QIndirectTaxExtra
import qualified Lib.Finance.Storage.Queries.Invoice as QFInvoice
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as HQPaymentTransaction
import qualified SharedLogic.RenderInvoiceFromTemplate as RIFT
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Person as QPerson
import "beckn-services" Tools.InvoicePdf (generateFinanceInvoicePdf)
import qualified Utils.QRCode.Encoder as QREncoder

-- | Build the invoice HTML and render it to a base64 PDF. Pure lookups off the
--   invoice row — no auth context needed, so it works from a background trigger.
-- | Encode the invoice's QR payload (signed IRP QR for B2B, else the
--   self-generated B2C unsigned QR) into a base64 PNG @data:@ URI for the
--   invoice template. Nothing when neither QR is present or encoding fails.
mkInvoiceQrDataUri :: MonadIO m => FinanceInvoice.Invoice -> m (Maybe Text)
mkInvoiceQrDataUri inv =
  case inv.signedQRCode <|> inv.unsignedQRCode of
    Nothing -> pure Nothing
    Just content -> liftIO (QREncoder.encodeQRCodePngDataUri content)

-- | Pre-signed S3 GET URL for an already-materialised invoice PDF, for the
--   invoice-list responses. Returns a full self-expiring @https@ URL that works
--   against a private bucket, or Nothing until the PDF exists in S3 (see
--   'generateAndStoreInvoicePdf' / the read-path write-through). Never renders,
--   so it stays cheap per list item (SigV4 signing is local). The 1-hour window
--   gives a user browsing the list time to click before the link expires.
mkInvoicePdfUrl :: FinanceInvoice.Invoice -> Flow (Maybe Text)
mkInvoicePdfUrl inv = case inv.pdfS3Path of
  Just path -> Just <$> S3.generateDownloadUrl (T.unpack path) invoicePdfListUrlTtl
  Nothing -> pure Nothing
  where
    invoicePdfListUrlTtl = Seconds 3600 -- 1 hour

renderInvoicePdfBase64 :: FinanceInvoice.Invoice -> Flow Text
renderInvoicePdfBase64 inv = do
  mbDriver <- QPerson.findById (Id inv.issuedToId :: Id DP.Person)
  mbTransporterConfig <-
    getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = inv.merchantOperatingCityId}) Nothing

  let items = parseLineItems inv.lineItems

  taxTxns <- QIndirectTaxExtra.findByInvoiceNumber (Just inv.invoiceNumber)
  let mbTaxTxn = listToMaybe taxTxns

  (mbPayType, mbBrand, mbLast4) <- case inv.entityReferenceId of
    Just orderId -> do
      txns <- HQPaymentTransaction.findAllByOrderId (Id orderId)
      let mbTxn = listToMaybe txns
      pure (mbTxn >>= (.paymentMethodType), mbTxn >>= (.cardBrand), mbTxn >>= (.cardLastFourDigits))
    Nothing -> pure (Nothing, Nothing, Nothing)

  -- AggregatedCommission party metadata (not persisted on the row).
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

  mbQrDataUri <- mkInvoiceQrDataUri inv

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
              mbSellerVatNumber = mbSellerVat,
              reverseCharge = mbTransporterConfig >>= (.invoiceConfig) >>= (.reverseCharge),
              ecoName = mbTransporterConfig >>= (.invoiceConfig) >>= (.ecoName),
              ecoAddress = mbTransporterConfig >>= (.invoiceConfig) >>= (.ecoAddress),
              ecoGstin = mbTransporterConfig >>= (.invoiceConfig) >>= (.ecoGstin),
              hsnSacCode = mbTransporterConfig >>= (.invoiceConfig) >>= (.hsnSacCode),
              categoryOfServices = mbTransporterConfig >>= (.invoiceConfig) >>= (.categoryOfServices),
              signatureImageUrl = mbTransporterConfig >>= (.invoiceConfig) >>= (.signatureImageUrl) <&> showBaseUrl,
              cityState = mbTransporterConfig >>= (.invoiceConfig) >>= (.cityState),
              qrImageDataUri = mbQrDataUri
            }
      mbInvType = case inv.invoiceType of
        AggregatedCommission -> Just AggregatedCommission
        _ -> Nothing
  html <- RIFT.renderHtml (Id inv.merchantOperatingCityId) mbInvType lang tz ctx
  generateFinanceInvoicePdf inv.invoiceNumber html

-- | Upload an already-rendered base64 PDF to S3 and stamp 'pdfS3Path' on the row.
--   Used both as the write-through on read and by 'generateAndStoreInvoicePdf'.
storeInvoicePdf :: FinanceInvoice.Invoice -> Text -> Flow ()
storeInvoicePdf inv pdfBase64 = do
  filePath <- S3.createFilePath "/finance-invoices/" ("invoice-" <> inv.id.getId) S3.PDF "pdf"
  S3.put (T.unpack filePath) pdfBase64
  QFInvoice.updatePdfS3Path (Just filePath) Nothing Nothing inv.id
  logInfo $ "Stored invoice PDF for " <> inv.id.getId <> " at " <> filePath

-- | Render + store, keyed on invoice id. Idempotent (skips if already stored) and
--   never throws. For eager / ONDC-push materialisation from a Flow context.
generateAndStoreInvoicePdf :: Id FinanceInvoice.Invoice -> Flow ()
generateAndStoreInvoicePdf invoiceId = do
  eRes <- withTryCatch "generateAndStoreInvoicePdf" $ do
    mbInv <- QFInvoice.findById invoiceId
    whenJust mbInv $ \inv ->
      when (isNothing inv.pdfS3Path) $ do
        pdfBase64 <- renderInvoicePdfBase64 inv
        storeInvoicePdf inv pdfBase64
  case eRes of
    Left err -> logError $ "generateAndStoreInvoicePdf failed for " <> invoiceId.getId <> ": " <> show err
    Right _ -> pure ()

-- | For ONDC on_status documents[]: ensure the booking's invoice PDF is in S3, then
--   return a short-lived pre-signed GET URL. Nothing when there's no invoice or the
--   render/store failed (the document is simply omitted from on_status in that case).
--   The BAP is expected to fetch + cache within the window (S3 enforces expiry via SigV4).
getInvoiceDocumentUrl :: DRB.Booking -> Flow (Maybe Text)
getInvoiceDocumentUrl booking = case booking.financeInvoiceId of
  Nothing -> pure Nothing
  Just invIdText -> do
    let invId = Id invIdText
    generateAndStoreInvoicePdf invId -- idempotent: materialise the PDF in S3 if not already
    mbInv <- QFInvoice.findById invId
    case mbInv >>= (.pdfS3Path) of
      Just path -> Just <$> S3.generateDownloadUrl (T.unpack path) invoiceUrlTtlSeconds
      Nothing -> pure Nothing
  where
    invoiceUrlTtlSeconds = Seconds 300 -- 5 minutes; BAP fetches + caches on receipt of on_status
