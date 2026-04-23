{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.EInvoice
  ( generateEInvoiceForInvoice,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (utctDay)
import qualified Domain.Types.DriverGstin as DDriverGstin
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as Person
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.GSTEInvoice.CharteredInfo.Types as CITypes
import qualified Kernel.External.GSTEInvoice.Interface as GSTEInvoice
import Kernel.External.GSTEInvoice.Interface.Types (GSTEInvoiceConfig (..))
import qualified Kernel.External.GSTEInvoice.Types as GSTEInvoiceTypes
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as IndirectTax
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import Lib.Finance.Invoice.Interface (InvoiceLineItem)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransactionExtra as QIndirectTax
import qualified Lib.Finance.Storage.Queries.Invoice as QFInvoice
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.Queries.DriverGstin as QDriverGstin

-- | Attempt B2B e-invoice generation for a created invoice.
--
--   Flow:
--     1. Look up the driver's GSTIN record (Storage.Queries.DriverGstin.findByDriverId).
--        If absent — B2C driver, skip.
--     2. Fetch the GSTEInvoice service config for the invoice's operating city.
--        If absent — service not configured for this city, skip.
--     3. Decrypt the driver's GSTIN, obtain a (cached) auth token, build the
--        payload from the DriverGstin row, and call generateEInvoice.
--
--   Errors are logged but never thrown — invoice creation must not fail because
--   of downstream e-invoice problems; failed IRNs can be retried separately.
generateEInvoiceForInvoice ::
  ( Redis.HedisFlow m r,
    EncFlow m r,
    Metrics.CoreMetrics m,
    HasRequestId r,
    BeamFlow.BeamFlow m r
  ) =>
  FInvoice.Invoice ->
  m ()
generateEInvoiceForInvoice invoice = do
  let ctx =
        " invoiceId=" <> invoice.id.getId
          <> " invoiceNumber="
          <> invoice.invoiceNumber
          <> " issuedToId="
          <> invoice.issuedToId
          <> " merchantOpCityId="
          <> invoice.merchantOperatingCityId
  logInfo $ "GSTEInvoice: starting e-invoice flow." <> ctx
  mbDriverGstin <- QDriverGstin.findByDriverId (Id invoice.issuedToId :: Id Person.Person)
  case mbDriverGstin of
    Nothing -> logInfo $ "GSTEInvoice: no DriverGstin for driver — treating as B2C, skipping e-invoice." <> ctx
    Just dg -> do
      logInfo $ "GSTEInvoice: found DriverGstin record gstinId=" <> dg.id.getId <> ctx
      mbServiceConfig <-
        CQMSC.findByServiceAndCity
          (DMSC.GSTEInvoiceService GSTEInvoiceTypes.CharteredInfo)
          (Id invoice.merchantOperatingCityId)
      let mbCfg = do
            sc <- mbServiceConfig
            case sc.serviceConfig of
              DMSC.GSTEInvoiceServiceConfig cfg -> Just cfg
              _ -> Nothing
      case mbCfg of
        Nothing ->
          logInfo $
            "GSTEInvoice: no GSTEInvoice service config for operating city — skipping e-invoice. hasRawServiceConfig="
              <> show (isJust mbServiceConfig)
              <> ctx
        Just cfg -> do
          logInfo $ "GSTEInvoice: B2B transaction detected, generating e-invoice." <> ctx
          eResult <- try @_ @SomeException $ case cfg of
            CharteredInfoEInvoiceConfig ciCfg -> do
              logDebug $ "GSTEInvoice: using Chartered Info GSP sellerGstin=" <> ciCfg.gstin <> ctx
              decryptedGstin <- decrypt dg.gstin
              logDebug $ "GSTEInvoice: decrypted buyer GSTIN, stateCode=" <> gstinStateCode decryptedGstin <> ctx
              logDebug $ "GSTEInvoice: fetching auth token." <> ctx
              authToken <- getOrRefreshAuthToken cfg
              logDebug $
                "GSTEInvoice: auth token obtained, tokenLength="
                  <> show (T.length authToken)
                  <> ctx
              mbIndirectTax <- listToMaybe <$> QIndirectTax.findByInvoiceNumber invoice.invoiceNumber
              logInfo $
                "GSTEInvoice: indirect-tax lookup result hasRecord="
                  <> show (isJust mbIndirectTax)
                  <> maybe "" (\t -> " taxRate=" <> show t.taxRate <> " taxableValue=" <> show t.taxableValue) mbIndirectTax
                  <> ctx
              let payload = buildEInvoicePayload invoice dg decryptedGstin mbIndirectTax ciCfg
              logDebug $ "GSTEInvoice: built payload, calling GSP generateEInvoice." <> ctx
              eInvResp <- GSTEInvoice.generateEInvoice cfg authToken payload
              logInfo $
                "GSTEInvoice: GSP responded status="
                  <> eInvResp.status
                  <> " irn="
                  <> fromMaybe "<no IRN in response>" eInvResp.irn
                  <> " ackNo="
                  <> fromMaybe "<none>" eInvResp.ackNo
                  <> " ackDt="
                  <> fromMaybe "<none>" eInvResp.ackDt
                  <> ctx
              logError $ "GSTEInvoice: IRN generated: " <> fromMaybe "<no IRN in response>" eInvResp.irn
              QFInvoice.updateIrnByInvoiceId eInvResp.irn invoice.id
              logInfo $ "GSTEInvoice: invoice row updated with IRN." <> ctx
          case eResult of
            Right _ -> logInfo $ "GSTEInvoice: e-invoice flow completed successfully." <> ctx
            Left err -> logError $ "GSTEInvoice: e-invoice generation failed: " <> show err <> ctx

-- | Build the e-invoice payload from an Invoice plus the buyer's DriverGstin record.
--   buyerDtls is populated entirely from DriverGstin; the platform (seller)
--   details come from the CharteredInfoConfig.
buildEInvoicePayload ::
  FInvoice.Invoice ->
  DDriverGstin.DriverGstin ->
  Text -> -- decrypted driver GSTIN
  Maybe IndirectTax.IndirectTaxTransaction ->
  CITypes.CharteredInfoConfig ->
  CITypes.EInvoicePayload
buildEInvoicePayload inv dg decryptedGstin mbIndirectTax CITypes.CharteredInfoConfig {..} =
  let hsnCode = fromMaybe "" (mbIndirectTax >>= (.sacCode))
      gstRate = fromMaybe 18 (mbIndirectTax >>= (.taxRate))
      igstAmount = maybe 0 (realToFrac . (.igstAmount)) mbIndirectTax
      cgstAmount = maybe 0 (realToFrac . (.cgstAmount)) mbIndirectTax
      sgstAmount = maybe 0 (realToFrac . (.sgstAmount)) mbIndirectTax
      lineItems = case Aeson.fromJSON inv.lineItems of
        Aeson.Success xs -> xs :: [InvoiceLineItem]
        Aeson.Error _ -> []
      assessableValue = realToFrac (sum $ map (.lineTotal) lineItems)
      totalInvoiceValue = realToFrac inv.totalAmount
   in CITypes.EInvoicePayload
        { version = "1.1",
          tranDtls =
            CITypes.TranDtls
              { taxSch = "GST",
                supTyp = "B2B"
              },
          docDtls =
            CITypes.DocDtls
              { typ = "INV",
                no = inv.invoiceNumber,
                dt = formatInvoiceDate inv.issuedAt
              },
          sellerDtls =
            CITypes.SellerDtls
              { gstin = gstin,
                lglNm = sellerGstinLegalName,
                addr1 = sellerGstinAddr,
                loc = sellerGstinLocation,
                pin = sellerGstinPinCode,
                stcd = sellerGstinStcd
              },
          buyerDtls =
            CITypes.BuyerDtls
              { gstin = decryptedGstin,
                lglNm = fromMaybe (fromMaybe "" inv.issuedToName) dg.legalName,
                addr1 = fromMaybe (fromMaybe "" inv.issuedToAddress) dg.address,
                loc = fromMaybe (fromMaybe (fromMaybe "" inv.issuedToAddress) dg.address) dg.stateName,
                pin = fromMaybe 0 (dg.pincode >>= readMaybe . T.unpack),
                stcd = gstinStateCode decryptedGstin,
                pos = Just (gstinStateCode decryptedGstin)
              },
          itemList =
            [ CITypes.ItemEntry
                { slNo = "1",
                  isServc = "Y",
                  hsnCd = hsnCode,
                  qty = 1,
                  unit = "NOS",
                  unitPrice = assessableValue,
                  totAmt = assessableValue,
                  discount = 0,
                  preTaxVal = 0,
                  assAmt = assessableValue,
                  gstRt = gstRate,
                  igstAmt = igstAmount,
                  cgstAmt = cgstAmount,
                  sgstAmt = sgstAmount,
                  totItemVal = totalInvoiceValue
                }
            ],
          valDtls =
            CITypes.ValDtls
              { assVal = assessableValue,
                cgstVal = cgstAmount,
                sgstVal = sgstAmount,
                igstVal = igstAmount,
                totInvVal = totalInvoiceValue
              },
          ewbDtls = Nothing
        }

-- | Extract the 2-digit state code from a GSTIN. A GSTIN is structured as
--   <2-digit state code><10-char PAN><entity-no><Z><checksum>, so the first
--   two characters identify the registering state.
gstinStateCode :: Text -> Text
gstinStateCode = T.take 2

-- | Format UTCTime to DD/MM/YYYY for e-invoice DocDtls.
formatInvoiceDate :: UTCTime -> Text
formatInvoiceDate t =
  let (y, m, d) = toGregorian (utctDay t)
      pad2 n = if n < 10 then "0" <> show n else show n
   in pad2 d <> "/" <> pad2 m <> "/" <> show y

-- | Get a cached GST e-invoice auth token from Redis, or authenticate and cache a new one.
--   TTL is set to (tokenExpiry - 60) seconds for safety.
--   The Redis key is scoped per-config (aspId + gstin + userName) so that
--   different merchant credential sets that share a GSTIN do not collide.
getOrRefreshAuthToken ::
  ( Redis.HedisFlow m r,
    EncFlow m r,
    Metrics.CoreMetrics m,
    HasRequestId r,
    MonadFlow m
  ) =>
  GSTEInvoiceConfig ->
  m Text
getOrRefreshAuthToken config = do
  let (gstin, scopeKey) = configScope config
      redisKey = "GSTEInvoice:authToken:" <> scopeKey
  cached <- Redis.get redisKey
  case cached of
    Just token -> do
      logInfo $ "GSTEInvoice: Using cached auth token for GSTIN " <> gstin
      pure token
    Nothing -> do
      logInfo $ "GSTEInvoice: Fetching new auth token for GSTIN " <> gstin
      authResp <- GSTEInvoice.authenticateEInvoice config
      let fallbackTtl = 3000 :: Int
      expirySeconds <- case readMaybe (T.unpack authResp.tokenExpiry) of
        Just s -> pure s
        Nothing -> do
          logWarning $
            "GSTEInvoice: failed to parse tokenExpiry="
              <> show authResp.tokenExpiry
              <> " for scope="
              <> scopeKey
              <> " (gstin="
              <> gstin
              <> "); falling back to "
              <> show fallbackTtl
              <> "s"
          pure fallbackTtl
      let ttl = max 3000 (expirySeconds - 60)
      Redis.setExp redisKey authResp.authToken ttl
      pure authResp.authToken
  where
    -- Returns (gstin-for-logging, per-config unique scope key for Redis).
    configScope :: GSTEInvoiceConfig -> (Text, Text)
    configScope (CharteredInfoEInvoiceConfig cfg) =
      (cfg.gstin, cfg.aspId <> ":" <> cfg.gstin <> ":" <> cfg.userName)
