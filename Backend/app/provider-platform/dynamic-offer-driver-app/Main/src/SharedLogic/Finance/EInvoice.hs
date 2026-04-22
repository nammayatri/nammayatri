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
  mbDriverGstin <- QDriverGstin.findByDriverId (Id invoice.issuedToId :: Id Person.Person)
  case mbDriverGstin of
    Nothing -> pure ()
    Just dg -> do
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
        Nothing -> pure ()
        Just cfg -> do
          logInfo "GSTEInvoice: B2B transaction detected, generating e-invoice"
          eResult <- try @_ @SomeException $ case cfg of
            CharteredInfoEInvoiceConfig ciCfg -> do
              decryptedGstin <- decrypt dg.gstin
              authToken <- getOrRefreshAuthToken cfg
              mbIndirectTax <- listToMaybe <$> QIndirectTax.findByInvoiceNumber invoice.invoiceNumber
              let payload = buildEInvoicePayload invoice dg decryptedGstin mbIndirectTax ciCfg
              eInvResp <- GSTEInvoice.generateEInvoice cfg authToken payload
              logError $ "GSTEInvoice: IRN generated: " <> fromMaybe "<no IRN in response>" eInvResp.irn
              QFInvoice.updateIrnByInvoiceId eInvResp.irn invoice.id
          case eResult of
            Right _ -> pure ()
            Left err -> logError $ "GSTEInvoice: e-invoice generation failed: " <> show err

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
  let gstin = gstinFromConfig config
      redisKey = "GSTEInvoice:authToken:" <> gstin
  cached <- Redis.get redisKey
  case cached of
    Just token -> do
      logInfo $ "GSTEInvoice: Using cached auth token for GSTIN " <> gstin
      pure token
    Nothing -> do
      logInfo $ "GSTEInvoice: Fetching new auth token for GSTIN " <> gstin
      authResp <- GSTEInvoice.authenticateEInvoice config
      let expirySeconds = fromMaybe 3600 (readMaybe (T.unpack authResp.tokenExpiry))
          ttl = max 60 (expirySeconds - 60)
      Redis.setExp redisKey authResp.authToken ttl
      pure authResp.authToken
  where
    gstinFromConfig :: GSTEInvoiceConfig -> Text
    gstinFromConfig (CharteredInfoEInvoiceConfig cfg) = cfg.gstin
