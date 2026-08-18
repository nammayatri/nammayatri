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
    getInvoicePdfPresignedUrl,
    materialiseInvoicePdfAsync,
    mkInvoiceQrDataUri,
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

-- | For ONDC on_status documents[]: return a short-lived pre-signed GET URL for the
--   booking's invoice PDF — but only when it is *already* materialised in S3.
--   Read-only by design: this runs inline on the BECKN on_status ACK path, so it must
--   never fork wkhtmltopdf (multi-second render → ONDC ACK-latency NACK + OOM risk).
--   The PDF is materialised out-of-band at ride-end / cancellation via
--   'materialiseInvoicePdfAsync'. Nothing when there's no invoice or it isn't stored yet
--   (the document is simply omitted from on_status until a later poll picks it up).
getInvoiceDocumentUrl :: DRB.Booking -> Flow (Maybe Text)
getInvoiceDocumentUrl booking = case booking.financeInvoiceId of
  Nothing -> pure Nothing
  Just invIdText -> do
    let invoiceId = Id invIdText
    mbInv <- QFInvoice.findById invoiceId
    case mbInv >>= (.pdfS3Path) of
      Just path -> Just <$> S3.generateDownloadUrl (T.unpack path) invoiceUrlTtlSeconds
      Nothing -> do
        -- Not materialised yet: kick off an out-of-band render (idempotent, non-blocking,
        -- gated on PDF-storage opt-in) so a subsequent on_status poll can presign it. The
        -- document is simply omitted from this on_status until then.
        whenJust mbInv $ \_ -> materialiseInvoicePdfAsync booking.merchantOperatingCityId.getId invoiceId
        pure Nothing
  where
    invoiceUrlTtlSeconds = Seconds 300 -- 5 minutes; BAP fetches + caches on receipt of on_status

-- | Resolve a short-lived presigned GET URL for an invoice's stored PDF, keyed by
--   invoice id. Presigns the S3 object when already materialised; otherwise — and only
--   when the merchant opted into PDF storage — renders + stores it on demand
--   (write-through) so subsequent calls are cheap presigns. Nothing when the invoice
--   does not exist, PDF storage is disabled, or the render/store failed. Callers are
--   responsible for authorizing access to @invoiceId@ before calling.
getInvoicePdfPresignedUrl :: Id FinanceInvoice.Invoice -> Flow (Maybe Text)
getInvoicePdfPresignedUrl invoiceId = do
  mbInv <- QFInvoice.findById invoiceId
  case mbInv of
    Nothing -> pure Nothing
    Just inv -> do
      mbPath <- case inv.pdfS3Path of
        Just path -> pure (Just path)
        Nothing -> do
          mbTransporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = inv.merchantOperatingCityId}) Nothing
          if fromMaybe False (mbTransporterConfig >>= (.invoiceConfig) >>= (.enableInvoicePdfS3Storage))
            then do
              generateAndStoreInvoicePdf invoiceId -- idempotent + never throws; stamps pdfS3Path
              (>>= (.pdfS3Path)) <$> QFInvoice.findById invoiceId
            else pure Nothing
      forM mbPath $ \path -> S3.generateDownloadUrl (T.unpack path) invoicePresignTtlSeconds
  where
    invoicePresignTtlSeconds = Seconds 300 -- 5 minutes; enough for a click-to-download

-- | Materialise the invoice PDF to S3 out-of-band (forked) so a later ONDC on_status
--   poll can presign it without a synchronous render on the ACK path. No-op unless the
--   merchant opted into PDF storage. Idempotent + never throws (delegates to
--   'generateAndStoreInvoicePdf'). Fetches the (cache-backed) transporter config to
--   evaluate the opt-in gate, since the on_status read path has no config in scope.
materialiseInvoicePdfAsync :: Text -> Id FinanceInvoice.Invoice -> Flow ()
materialiseInvoicePdfAsync mocId invoiceId = do
  mbTransporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mocId}) Nothing
  when (fromMaybe False (mbTransporterConfig >>= (.invoiceConfig) >>= (.enableInvoicePdfS3Storage))) $
    fork "materialiseInvoicePdf" $ generateAndStoreInvoicePdf invoiceId
