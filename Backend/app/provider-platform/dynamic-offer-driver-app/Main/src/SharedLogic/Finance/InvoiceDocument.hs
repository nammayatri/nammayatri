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
    getInvoicePdfBase64,
    generateAndStoreInvoicePdf,
    getInvoiceDocumentUrl,
    getStoredInvoiceDocumentUrl,
    getInvoicePdfPresignedUrl,
    interactivePresignTtl,
    mkInvoiceQrDataUri,
  )
where

import qualified AWS.S3 as S3
import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
--   The object holds the decoded PDF bytes with an @application/pdf@ content type,
--   so a presigned URL serves a usable file.
storeInvoicePdf :: FinanceInvoice.Invoice -> Text -> Flow ()
storeInvoicePdf inv pdfBase64 = do
  filePath <- S3.createFilePath "/finance-invoices/" ("invoice-" <> inv.id.getId) S3.PDF "pdf"
  S3.putRaw (T.unpack filePath) (B64.decodeLenient (TE.encodeUtf8 pdfBase64)) "application/pdf"
  QFInvoice.updatePdfS3Path (Just filePath) Nothing Nothing inv.id
  logInfo $ "Stored invoice PDF for " <> inv.id.getId <> " at " <> filePath

-- | Base64 PDF for an invoice: the stored S3 object when present, else rendered on
--   demand. A stored object that cannot be read (expired, migrated, half-written)
--   falls back to rendering. @storeOnRender@ persists a freshly rendered PDF
--   (write-through) so later reads and presigned URLs are served from S3.
getInvoicePdfBase64 :: Bool -> FinanceInvoice.Invoice -> Flow Text
getInvoicePdfBase64 storeOnRender inv = do
  mbStored <- case inv.pdfS3Path of
    Nothing -> pure Nothing
    Just path -> do
      eBytes <- withTryCatch "getInvoicePdfBase64:s3Get" $ S3.getRaw (T.unpack path)
      case eBytes of
        Left err -> do
          logError $ "Stored invoice PDF unreadable for " <> inv.id.getId <> " at " <> path <> ", re-rendering: " <> show err
          pure Nothing
        Right bytes
          -- Objects written before the putRaw switch hold base64 text, not PDF bytes.
          | "%PDF" `BS.isPrefixOf` bytes -> pure . Just . TE.decodeUtf8 $ B64.encode bytes
          | otherwise -> pure . Just $ TE.decodeUtf8 bytes
  case mbStored of
    Just pdf -> pure pdf
    Nothing -> do
      pdf <- renderInvoicePdfBase64 inv
      when storeOnRender $ storeInvoicePdf inv pdf
      pure pdf

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

-- | Presign TTL for a click-to-download link handed straight to a dashboard / app user.
interactivePresignTtl :: Seconds
interactivePresignTtl = Seconds 300

-- | Presign TTL for a link shared with the BAP in ONDC @order.documents[]@, which the
--   BAP persists and shows whenever the rider opens their receipt. 7 days is the SigV4
--   maximum; a presign signed with temporary (STS / instance-role) credentials still
--   expires when those credentials do.
ondcDocumentPresignTtl :: Seconds
ondcDocumentPresignTtl = Seconds 604800

-- | ONDC on_cancel: presigned URL for the booking's invoice, rendering + storing it
--   on demand when the merchant opted into PDF storage. Never throws — a failure
--   just omits the document. Call it off the ACK path (it may render a PDF).
getInvoiceDocumentUrl :: DRB.Booking -> Flow (Maybe Text)
getInvoiceDocumentUrl booking = case booking.financeInvoiceId of
  Nothing -> pure Nothing
  Just invIdText ->
    neverThrow "getInvoiceDocumentUrl" invIdText $
      getInvoicePdfPresignedUrl ondcDocumentPresignTtl (Id invIdText)

-- | ONDC on_status: presigned URL for the booking's invoice only when its PDF is
--   already stored. Never renders (the /status request is on the Beckn ACK path) and
--   never throws — a failure just omits the document.
getStoredInvoiceDocumentUrl :: DRB.Booking -> Flow (Maybe Text)
getStoredInvoiceDocumentUrl booking = case booking.financeInvoiceId of
  Nothing -> pure Nothing
  Just invIdText ->
    neverThrow "getStoredInvoiceDocumentUrl" invIdText $ do
      mbInv <- QFInvoice.findById (Id invIdText)
      forM (mbInv >>= (.pdfS3Path)) $ \path ->
        S3.generateDownloadUrl (T.unpack path) ondcDocumentPresignTtl

neverThrow :: Text -> Text -> Flow (Maybe Text) -> Flow (Maybe Text)
neverThrow tag invIdText action = do
  eRes <- withTryCatch tag action
  case eRes of
    Left err -> do
      logError $ tag <> " failed for invoice " <> invIdText <> ": " <> show err
      pure Nothing
    Right res -> pure res

-- | Resolve a presigned GET URL for an invoice's stored PDF, keyed by invoice id.
--   Presigns the S3 object when already materialised; otherwise — and only when the
--   merchant opted into PDF storage — renders + stores it on demand (write-through) so
--   subsequent calls are cheap presigns. Nothing when the invoice does not exist, PDF
--   storage is disabled, or the render/store failed. Callers are responsible for
--   authorizing access to @invoiceId@ before calling.
getInvoicePdfPresignedUrl :: Seconds -> Id FinanceInvoice.Invoice -> Flow (Maybe Text)
getInvoicePdfPresignedUrl ttl invoiceId = do
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
      forM mbPath $ \path -> S3.generateDownloadUrl (T.unpack path) ttl
