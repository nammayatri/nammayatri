{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.EInvoice
  ( generateEInvoiceForInvoice,
    isEInvoiceEnabledForCity,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as AesonText
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (utctDay)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.LocalTime (localTimeToUTC, minutesToTimeZone)
import qualified Domain.Types.DriverGstin as DDriverGstin
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as Person
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.GSTEInvoice.CharteredInfo.Types as CITypes
import qualified Kernel.External.GSTEInvoice.Interface as GSTEInvoice
import Kernel.External.GSTEInvoice.Interface.Types (GSTEInvoiceConfig (..))
import qualified Kernel.External.GSTEInvoice.Interface.Types as Fields
import qualified Kernel.External.GSTEInvoice.Types as GSTEInvoiceTypes
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as IndirectTax
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import Lib.Finance.Invoice.Interface (InvoiceLineItem (..), LineItemDescription (..))
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransaction as QIndirectTax
import qualified Lib.Finance.Storage.Queries.Invoice as QFInvoice
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import qualified Storage.Queries.DriverGstin as QDriverGstin

isEInvoiceEnabledForCity ::
  (BeamFlow.BeamFlow m r) =>
  Text ->
  m Bool
isEInvoiceEnabledForCity merchantOpCityId = isJust <$> findEInvoiceConfig merchantOpCityId

-- | The city's e-invoice provider config: EY DigiGST when configured, else Chartered Info.
findEInvoiceConfig ::
  (BeamFlow.BeamFlow m r) =>
  Text ->
  m (Maybe GSTEInvoiceConfig)
findEInvoiceConfig merchantOpCityId = do
  mbDigiGST <- configFor GSTEInvoiceTypes.DigiGST
  maybe (configFor GSTEInvoiceTypes.CharteredInfo) (pure . Just) mbDigiGST
  where
    configFor service = do
      mbServiceConfig <-
        getOneConfig
          (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOpCityId, merchantId = Nothing, serviceName = Just (DMSC.GSTEInvoiceService service)})
          Nothing
      pure $ case mbServiceConfig of
        Just sc | DMSC.GSTEInvoiceServiceConfig cfg <- sc.serviceConfig -> Just cfg
        _ -> Nothing

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
    BeamFlow.BeamFlow m r,
    Finance.HasActorInfo m r
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
      mbCfg <- findEInvoiceConfig invoice.merchantOperatingCityId
      case mbCfg of
        Nothing -> logInfo $ "GSTEInvoice: no GSTEInvoice service config for operating city — skipping e-invoice." <> ctx
        Just cfg -> do
          logInfo $ "GSTEInvoice: B2B transaction detected, generating e-invoice." <> ctx
          eResult <- try @_ @SomeException $ do
            let seller = sellerGstinOf cfg
            logDebug $ "GSTEInvoice: using " <> providerName cfg <> " sellerGstin=" <> seller.sellerGstin <> ctx
            decryptedGstin <- decrypt dg.gstin
            logDebug $ "GSTEInvoice: decrypted buyer GSTIN, stateCode=" <> gstinStateCode decryptedGstin <> ctx
            logDebug $ "GSTEInvoice: fetching auth token." <> ctx
            authToken <- getOrRefreshAuthToken cfg
            logDebug $
              "GSTEInvoice: auth token obtained, tokenLength="
                <> show (T.length authToken)
                <> ctx
            mbIndirectTax <- listToMaybe <$> QIndirectTax.findByInvoiceNumber (Just invoice.invoiceNumber)
            logInfo $
              "GSTEInvoice: indirect-tax lookup result hasRecord="
                <> show (isJust mbIndirectTax)
                <> maybe "" (\t -> " taxRate=" <> show t.taxRate <> " taxableValue=" <> show t.taxableValue) mbIndirectTax
                <> ctx
            let payload = buildEInvoicePayload invoice dg decryptedGstin mbIndirectTax seller
            logDebug $
              "GSTEInvoice: built payload, calling GSP generateEInvoice. payload="
                <> TL.toStrict (AesonText.encodeToLazyText payload)
                <> ctx
            -- If the provider rejects the token (Chartered Info GSP752, DigiGST HTTP 401),
            -- the token died before/at use. Drop the cached token, re-authenticate,
            -- and retry the call ONCE.
            attempt1 <- try @_ @SomeException $ GSTEInvoice.generateEInvoice cfg authToken payload
            eInvResp <- case attempt1 of
              Right resp -> pure resp
              Left err ->
                if isAuthTokenExpiredError err
                  then do
                    logWarning $ "GSTEInvoice: provider rejected auth token; invalidating cache and retrying once." <> ctx
                    Redis.del (snd (authTokenCacheScope cfg))
                    freshToken <- getOrRefreshAuthToken cfg
                    logDebug $ "GSTEInvoice: retry with fresh auth token, tokenLength=" <> show (T.length freshToken) <> ctx
                    GSTEInvoice.generateEInvoice cfg freshToken payload
                  else throwM err
            logInfo $
              "GSTEInvoice: GSP responded status="
                <> eInvResp.status
                <> " irn="
                <> fromMaybe "<no IRN in response>" eInvResp.irn
                <> " ackNo="
                <> fromMaybe "<none>" eInvResp.ackNo
                <> " ackDt="
                <> fromMaybe "<none>" eInvResp.ackDt
                <> "SignesQRCode ="
                <> fromMaybe "<none>" eInvResp.signedQRCode
                <> ctx
            case eInvResp.irn of
              Just irn | eInvResp.status == "1" -> do
                logInfo $ "GSTEInvoice: IRN generated: " <> irn <> ctx
                actorInfo <- asks (.actorInfo)
                QFInvoice.updateIrnAndSignedQRByInvoiceId eInvResp.irn eInvResp.signedQRCode (Just actorInfo.actorType) actorInfo.actorId invoice.id
                QFInvoice.updatePdfS3Path Nothing (Just actorInfo.actorType) actorInfo.actorId invoice.id
                logInfo $ "GSTEInvoice: invoice row updated with IRN." <> ctx
              _ ->
                logError $
                  "GSTEInvoice: no IRN generated, invoice left unchanged. errors="
                    <> show (fromMaybe [] eInvResp.errors)
                    <> " info="
                    <> show (fromMaybe [] eInvResp.infoDtls)
                    <> ctx
          case eResult of
            Right _ -> logInfo $ "GSTEInvoice: e-invoice flow completed successfully." <> ctx
            Left err -> logError $ "GSTEInvoice: e-invoice generation failed: " <> show err <> ctx

-- | The platform (seller) GSTIN details, from whichever provider config the city uses.
data SellerGstin = SellerGstin
  { sellerGstin :: Text,
    sellerLegalName :: Text,
    sellerAddress :: Text,
    sellerLocation :: Text,
    sellerPinCode :: Int,
    sellerStateCode :: Text
  }

sellerGstinOf :: GSTEInvoiceConfig -> SellerGstin
sellerGstinOf (CharteredInfoEInvoiceConfig cfg) =
  SellerGstin cfg.gstin cfg.sellerGstinLegalName cfg.sellerGstinAddr cfg.sellerGstinLocation cfg.sellerGstinPinCode cfg.sellerGstinStcd
sellerGstinOf (DigiGSTEInvoiceConfig cfg) =
  SellerGstin cfg.gstin cfg.sellerGstinLegalName cfg.sellerGstinAddr cfg.sellerGstinLocation cfg.sellerGstinPinCode cfg.sellerGstinStcd

providerName :: GSTEInvoiceConfig -> Text
providerName (CharteredInfoEInvoiceConfig _) = "Chartered Info GSP"
providerName (DigiGSTEInvoiceConfig _) = "EY DigiGST"

-- | Build the e-invoice payload from an Invoice plus the buyer's DriverGstin record.
--   buyerDtls is populated entirely from DriverGstin; the seller details come
--   from the city's provider config.
buildEInvoicePayload ::
  FInvoice.Invoice ->
  DDriverGstin.DriverGstin ->
  Text -> -- decrypted driver GSTIN
  Maybe IndirectTax.IndirectTaxTransaction ->
  SellerGstin ->
  CITypes.EInvoicePayload
buildEInvoicePayload inv dg decryptedGstin mbIndirectTax SellerGstin {..} =
  let hsnCode = fromMaybe "" (mbIndirectTax >>= (.sacCode))
      gstRate = money3 $ fromMaybe 18 (mbIndirectTax >>= (.taxRate))
      igstAmount = money2 $ maybe 0 (realToFrac . (.igstAmount)) mbIndirectTax
      cgstAmount = money2 $ maybe 0 (realToFrac . (.cgstAmount)) mbIndirectTax
      sgstAmount = money2 $ maybe 0 (realToFrac . (.sgstAmount)) mbIndirectTax
      lineItems = case Aeson.fromJSON inv.lineItems of
        Aeson.Success xs -> xs :: [InvoiceLineItem]
        Aeson.Error err -> error $ "EInvoice.buildEInvoicePayload: invoice lineItems JSON failed to decode: " <> T.pack err
      -- Assessable value is the pre-tax principal only. Match by typed
      -- 'descriptionType' for new rows; fall back to legacy Text match so old
      -- rows (without descriptionType) still aggregate correctly.
      principalLineItems =
        filter
          (\li -> li.descriptionType == Just SubscriptionPlanFee || (isNothing li.descriptionType && li.description == "Subscription Plan Fee"))
          lineItems
      assessableValue = money2 $ realToFrac (sum $ map (.lineTotal) principalLineItems)
      totalInvoiceValue = money2 $ realToFrac inv.totalAmount
      zeroMoney = 0 :: Double
      buyerAddr = clampAddr1 $ fromMaybe (fromMaybe "" inv.issuedToAddress) dg.address
      sellerAddr = clampAddr1 sellerAddress
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
              { gstin = sellerGstin,
                lglNm = sellerLegalName,
                addr1 = sellerAddr,
                loc = sellerLocation,
                pin = sellerPinCode,
                stcd = sellerStateCode
              },
          buyerDtls =
            CITypes.BuyerDtls
              { gstin = decryptedGstin,
                lglNm = fromMaybe (fromMaybe "" inv.issuedToName) dg.legalName,
                addr1 = buyerAddr,
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
                  discount = zeroMoney,
                  preTaxVal = zeroMoney,
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
          ewbDtls = Just CITypes.EwbDtls {distance = 0}
        }

-- | Extract the 2-digit state code from a GSTIN. A GSTIN is structured as
--   <2-digit state code><10-char PAN><entity-no><Z><checksum>, so the first
--   two characters identify the registering state.
gstinStateCode :: Text -> Text
gstinStateCode = T.take 2

-- | Round a Double to 2 decimal places. The GSP regex `^\d+.?\d{0,2}$`
-- rejects any value with more than 2 decimals, so we round before sending.
money2 :: Double -> Double
money2 x = fromIntegral (round (x * 100) :: Integer) / 100

-- | Same as `money2` but with 3 decimal places (used for gstRt only).
money3 :: Double -> Double
money3 x = fromIntegral (round (x * 1000) :: Integer) / 1000

-- | GSP requires Address 1 to be 1-100 chars. Fall back to "NA" if empty, clip to 100.
clampAddr1 :: Text -> Text
clampAddr1 t =
  let trimmed = T.strip t
   in T.take 100 (if T.null trimmed then "NA" else trimmed)

-- | Format UTCTime to DD/MM/YYYY for e-invoice DocDtls.
formatInvoiceDate :: UTCTime -> Text
formatInvoiceDate t =
  let (y, m, d) = toGregorian (utctDay t)
      pad2 n = if n < 10 then "0" <> show n else show n
   in pad2 d <> "/" <> pad2 m <> "/" <> show y

-- | Get a cached GST e-invoice auth token from Redis, or authenticate and cache a new one.
--   TokenExpiry from the GSP is an ABSOLUTE IST wall-clock timestamp (not a
--   duration), so the cache TTL is set to (expiry - now - safetyMargin). This
--   guarantees a token is never served from Redis past its actual lifetime.
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
  let (gstin, redisKey) = authTokenCacheScope config
  cached <- Redis.get redisKey
  case cached of
    Just token -> do
      logInfo $ "GSTEInvoice: Using cached auth token for GSTIN " <> gstin
      pure token
    Nothing -> do
      logInfo $ "GSTEInvoice: Fetching new auth token for GSTIN " <> gstin
      authResp <- GSTEInvoice.authenticateEInvoice config
      now <- getCurrentTime
      -- TokenExpiry looks like "2026-07-20 17:03:26" and is an absolute IST
      -- (UTC+5:30) wall-clock time. Parse it and convert to UTC; DO NOT treat
      -- it as a seconds count (readMaybe @Int always fails on it).
      let istZone = minutesToTimeZone 330 -- IST = UTC+05:30
          safetyMargin = 60 :: Int
          -- If the expiry ever fails to parse, re-authenticate soon rather than
          -- caching a possibly-dead token for a long time.
          fallbackTtl = 300 :: Int
          mbExpiryUtc =
            localTimeToUTC istZone
              <$> parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack authResp.tokenExpiry)
      ttl <- case mbExpiryUtc of
        Just expiryUtc ->
          pure $ floor (realToFrac (diffUTCTime expiryUtc now) :: Double) - safetyMargin
        Nothing -> do
          logWarning $
            "GSTEInvoice: failed to parse tokenExpiry="
              <> show authResp.tokenExpiry
              <> " for scope="
              <> redisKey
              <> " (gstin="
              <> gstin
              <> "); falling back to "
              <> show fallbackTtl
              <> "s"
          pure fallbackTtl
      -- Only cache when the token has meaningful life left beyond the safety
      -- margin. A token at/near expiry is not cached, so the next call
      -- re-authenticates (cache miss) instead of serving a dead token.
      if ttl > 0
        then do
          logInfo $
            "GSTEInvoice: caching auth token for GSTIN "
              <> gstin
              <> " ttl="
              <> show ttl
              <> "s tokenExpiry="
              <> authResp.tokenExpiry
          Redis.setExp redisKey authResp.authToken ttl
        else
          logWarning $
            "GSTEInvoice: not caching auth token for GSTIN "
              <> gstin
              <> " — remaining life <= safety margin (tokenExpiry="
              <> authResp.tokenExpiry
              <> ")"
      pure authResp.authToken

-- | Returns (gstin-for-logging, per-config Redis key) for the e-invoice auth token.
--   Scoped by aspId + gstin + userName so different credential sets never collide.
authTokenCacheScope :: GSTEInvoiceConfig -> (Text, Text)
authTokenCacheScope (CharteredInfoEInvoiceConfig cfg) =
  (cfg.gstin, "GSTEInvoice:authToken:" <> cfg.aspId <> ":" <> cfg.gstin <> ":" <> cfg.userName)
authTokenCacheScope (DigiGSTEInvoiceConfig cfg) =
  (cfg.gstin, "GSTEInvoice:authToken:DigiGST:" <> cfg.userName <> ":" <> cfg.gstin)

-- | An expired token is thrown as an InternalError whose shown text embeds the
--   provider's reply: Chartered Info GSP752 ("AuthToken not found or expired"),
--   DigiGST HTTP 401 ("Invalid JWT token"). Detect it so we can invalidate + retry once.
isAuthTokenExpiredError :: SomeException -> Bool
isAuthTokenExpiredError e =
  let msg = T.pack (show e)
   in any (`T.isInfixOf` msg) ["GSP752", "AuthToken not found or expired", "statusCode = 401", "Invalid JWT token"]
