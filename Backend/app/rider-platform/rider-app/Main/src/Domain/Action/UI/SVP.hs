module Domain.Action.UI.SVP
  ( getSvpQr,
    postSvpGate,
  )
where

import qualified API.Types.UI.SVP as API
import qualified BecknV2.OnDemand.Enums as Spec
import qualified Crypto.Hash as Hash
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA.PKCS15
import qualified Crypto.Store.PKCS8 as PKCS8
import Data.Bits ((.&.), (.|.))
import qualified Data.ByteString.Base64 as B64
import Data.Char (isDigit, toUpper)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word32)
import Data.X509 (PrivKey (..))
import Domain.Types.IntegratedBPPConfig (ProviderConfig (..))
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SvpJourney as DSvp
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.GetFare as CMRLV2GetFare
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.External.Wallet.Interface.Types (WalletPostingReq (..), WalletPostingType (REDEEM))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Storage.Queries.PersonWallet as QPersonWallet
import Numeric (showHex)
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.Queries.Person as QPersonBase
import qualified Storage.Queries.PersonExtra as QPerson
import qualified Storage.Queries.SvpJourney as QSvpJourney
import Tools.Error
import qualified Tools.Wallet as TWallet

-- ── Redis keys ────────────────────────────────────────────

rsaPrivateKeyRedisKey :: Text
rsaPrivateKeyRedisKey = "SVP:RSAPrivateKeyPEM"

tktSlNoRedisKey :: Id DP.Person -> Text
tktSlNoRedisKey riderId = "SVP:TktSlNo:" <> getId riderId

tktSlNoCounterKey :: Text
tktSlNoCounterKey = "SVP:TktSlNoCounter"

toHexN :: (Integral a) => Int -> a -> Text
toHexN bytes val =
  let h = map toUpper $ showHex (toInteger val) ""
      padLen = bytes * 2
   in T.pack $ replicate (max 0 (padLen - length h)) '0' ++ take padLen h

encodeCoord :: Maybe Double -> Text
encodeCoord Nothing = toHexN 3 (0 :: Int)
encodeCoord (Just deg) = toHexN 3 (floor (abs deg * 1000) :: Int)

encodeMobile :: Text -> Text
encodeMobile m =
  let digits = T.filter isDigit m
   in T.justifyRight 10 '0' (T.takeEnd 10 digits)

nowEpochSec :: UTCTime -> Int
nowEpochSec = floor . utcTimeToPOSIXSeconds

buildTktSlNo :: UTCTime -> Int -> Text
buildTktSlNo now seq' =
  let dtHex = toHexN 4 (nowEpochSec now)
      seqWord = fromIntegral (0x40000000 .|. (seq' .&. 0x3FFFFFFF)) :: Word32
   in dtHex <> toHexN 4 seqWord

getOrCreateTktSlNo :: (CacheFlow m r, MonadFlow m) => Id DP.Person -> m Text
getOrCreateTktSlNo riderId = do
  mbSlNo <- Hedis.get (tktSlNoRedisKey riderId)
  case mbSlNo of
    Just slNo -> pure slNo
    Nothing -> do
      seq' <- Hedis.incr tktSlNoCounterKey
      now <- getCurrentTime
      let slNo = buildTktSlNo now (fromIntegral seq')
      Hedis.setExp (tktSlNoRedisKey riderId) slNo (8 * 3600)
      pure slNo

buildSqdsrPlaintext ::
  Text -> UTCTime -> Int -> Text -> Text -> Text -> Text -> Text
buildSqdsrPlaintext tktSlNo now balancePaisa txnRef mobile bookingLat bookingLon =
  let epoch = nowEpochSec now
      sec = toHexN 1 (0x03 :: Int)
      ver = toHexN 1 (0x04 :: Int)
      lang = toHexN 1 (0x00 :: Int)
      tgId = toHexN 2 (0x0001 :: Int)
      txnTy = toHexN 1 (0x41 :: Int)
      genDt = toHexN 4 epoch
      reqId = toHexN 4 (0x00000001 :: Int)
      txnRefPadded = T.justifyLeft 22 '0' (T.take 22 txnRef)
      totalFare = toHexN 4 (0 :: Int)
      svcFields =
        T.intercalate
          "|"
          [ sec,
            ver,
            lang,
            tgId,
            txnTy,
            tktSlNo,
            genDt,
            reqId,
            txnRefPadded,
            totalFare,
            bookingLat,
            bookingLon,
            mobile
          ]
      updTime = toHexN 4 epoch
      qrStatus = toHexN 2 (0x0087 :: Int) <> toHexN 1 (0x11 :: Int)
      opDyn = toHexN 4 balancePaisa <> toHexN 15 (0 :: Int)
      dynFields =
        T.intercalate
          "|"
          [updTime, qrStatus, toHexN 3 (0 :: Int), toHexN 3 (0 :: Int), opDyn]
      opId = toHexN 2 (0x0087 :: Int)
      noTkts = toHexN 1 (0x01 :: Int)
      valInfo = toHexN 1 (0x10 :: Int)
      grpSize = toHexN 1 (0x01 :: Int)
      srcStn = toHexN 2 (0x0000 :: Int)
      dstStn = toHexN 2 (0x0000 :: Int)
      actDt = toHexN 4 epoch
      prodId = toHexN 2 (0x0005 :: Int)
      svcId = toHexN 1 (0x01 :: Int)
      tktFare = toHexN 4 (0 :: Int)
      validity = toHexN 2 (0x01E0 :: Int)
      duration = toHexN 2 (0x00B4 :: Int)
      opTktData = toHexN 4 balancePaisa <> toHexN 4 (0x4E594D52 :: Int)
      tktFields =
        T.intercalate
          "|"
          [ grpSize,
            srcStn,
            dstStn,
            actDt,
            prodId,
            svcId,
            tktFare,
            validity,
            duration,
            opTktData
          ]
      tktPart =
        "(" <> opId <> "|" <> noTkts <> "|" <> valInfo
          <> "|["
          <> tktFields
          <> "])"
   in "{" <> svcFields <> "}|{" <> dynFields <> "}|{" <> tktPart <> "}"

generateAndStoreKeyPair :: (CacheFlow m r, MonadFlow m) => m RSA.PublicKey
generateAndStoreKeyPair = do
  (pub, priv) <- liftIO $ RSA.generate 256 0x10001
  Hedis.set rsaPrivateKeyRedisKey (encodePrivateKeyPem priv)
  pure pub

loadOrBootstrapKey :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => m RSA.PrivateKey
loadOrBootstrapKey = do
  mbPem <- Hedis.get rsaPrivateKeyRedisKey
  pem <- case mbPem of
    Just p -> pure p
    Nothing -> do
      logInfo "[SVP] No key in Redis — bootstrapping fresh RSA-2048 pair"
      void $ generateAndStoreKeyPair
      Hedis.get rsaPrivateKeyRedisKey
        >>= fromMaybeM (InternalError "SVP: key bootstrap failed")
  case parsePrivateKeyPem pem of
    Just k -> pure k
    Nothing -> do
      logError "[SVP] Bad RSA key PEM in Redis"
      throwError $ InternalError "SVP: invalid RSA key PEM"

signPlaintext :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> m Text
signPlaintext pt = do
  when (T.null pt) $ throwError (InvalidRequest "plaintext is required")
  privateKey <- loadOrBootstrapKey
  case RSA.PKCS15.sign Nothing (Just Hash.SHA256) privateKey (TE.encodeUtf8 pt) of
    Left err -> do
      logError $ "[SVP] Sign failed: " <> T.pack (show err)
      throwError (InternalError "SVP signing failed")
    Right sig -> pure $ TE.decodeUtf8 (B64.encode sig)

encodePrivateKeyPem :: RSA.PrivateKey -> Text
encodePrivateKeyPem priv = TE.decodeUtf8 $ PKCS8.writeKeyFileToMemory PKCS8.PKCS8Format [PrivKeyRSA priv]

parsePrivateKeyPem :: Text -> Maybe RSA.PrivateKey
parsePrivateKeyPem pem = case PKCS8.readKeyFileFromMemory (TE.encodeUtf8 pem) of
  (PKCS8.Unprotected (PrivKeyRSA k) : _) -> Just k
  _ -> Nothing

getSvpQr ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m, EncFlow m r) =>
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Maybe Double ->
  Maybe Double ->
  m API.GenerateQrResp
getSvpQr (Just riderId, _) mbLat mbLon = do
  tktSlNo' <- getOrCreateTktSlNo riderId
  person <- QPersonBase.findById riderId >>= fromMaybeM (PersonNotFound (getId riderId))
  decMobile <- mapM decrypt person.mobileNumber
  let mobile = encodeMobile (fromMaybe "0000000000" decMobile)
  mbWallet <- QPersonWallet.findByPersonId (getId riderId)
  let balancePaisa = case mbWallet of
        Just pw -> floor (getHighPrecMoney pw.usableCashAmount * 100) :: Int
        Nothing -> 0
  let txnRef = T.take 22 $ T.filter (/= '-') (getId riderId)
      bookingLat = encodeCoord mbLat
      bookingLon = encodeCoord mbLon
  now <- getCurrentTime
  let plaintext = buildSqdsrPlaintext tktSlNo' now balancePaisa txnRef mobile bookingLat bookingLon
  sig <- signPlaintext plaintext
  let qrPayload = plaintext <> "|{SIG:" <> sig <> "}"
  mbJourney <- QSvpJourney.findByRiderIdAndStatus riderId DSvp.ENTERED
  pure
    API.GenerateQrResp
      { qrData = qrPayload,
        tktSlNo = tktSlNo',
        validTill = addUTCTime (8 * 3600) now,
        journeyStatus = (.status) <$> mbJourney
      }
getSvpQr (Nothing, _) _ _ = throwError $ InvalidRequest "Rider not found in token"

svpMinFarePaisa :: Int
svpMinFarePaisa = 5000

postSvpGate ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  API.GateCallbackReq ->
  m API.GateCallbackResp
postSvpGate API.GateCallbackReq {..} = case scanType of
  API.ENTRY -> handleEntry mobileNumber merchantId stationCode timestamp
  API.EXIT -> handleExit mobileNumber merchantId stationCode timestamp

findRiderByMobile ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) =>
  Text ->
  Id DM.Merchant ->
  m DP.Person
findRiderByMobile mobileNum mId = do
  mobileHash <- getDbHash mobileNum
  -- Use role-based lookup to avoid country code format mismatch ("+91" vs "91")
  QPerson.findByMobileNumberAndMerchantAndRole mobileHash mId [DP.USER]
    >>= fromMaybeM (PersonNotFound $ "mobile=" <> mobileNum)

handleEntry ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m, EncFlow m r) =>
  Text ->
  Id DM.Merchant ->
  Text ->
  UTCTime ->
  m API.GateCallbackResp
handleEntry mobileNum mId stationCode entryTime = do
  person <- findRiderByMobile mobileNum mId
  mbWallet <- QPersonWallet.findByPersonId (getId person.id)
  let balance = case mbWallet of
        Just pw -> floor (getHighPrecMoney pw.usableCashAmount * 100) :: Int
        Nothing -> 0
  if balance < svpMinFarePaisa
    then
      pure
        API.GateCallbackResp
          { allowed = False,
            reason =
              Just $
                "Insufficient balance. Minimum ₹"
                  <> show (svpMinFarePaisa `div` 100)
                  <> " required."
          }
    else do
      mbTktSlNo <- Hedis.get (tktSlNoRedisKey person.id)
      tktSlNo' <- fromMaybeM (InvalidRequest "SVP: QR not generated. Please open the app and generate a QR first.") mbTktSlNo
      svpJourneyId <- generateGUID
      now <- getCurrentTime
      QSvpJourney.create
        DSvp.SvpJourney
          { id = Id svpJourneyId,
            riderId = person.id,
            tktSlNo = tktSlNo',
            status = DSvp.ENTERED,
            entryStationCode = Just stationCode,
            entryTime = Just entryTime,
            exitStationCode = Nothing,
            exitTime = Nothing,
            fareCharged = Nothing,
            currency = Nothing,
            merchantId = person.merchantId,
            merchantOperatingCityId = person.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }
      logInfo $
        "[SVP:Entry] rider=" <> getId person.id
          <> " station="
          <> stationCode
          <> " balance=₹"
          <> show (balance `div` 100)
      pure API.GateCallbackResp {allowed = True, reason = Nothing}

handleExit ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  Text ->
  Id DM.Merchant ->
  Text ->
  UTCTime ->
  m API.GateCallbackResp
handleExit mobileNum mId stationCode exitTime = do
  person <- findRiderByMobile mobileNum mId
  mbJourney <- QSvpJourney.findByRiderIdAndStatus person.id DSvp.ENTERED
  journey <- fromMaybeM (InvalidRequest "SVP: no active journey") mbJourney
  let entryStation = fromMaybe "UNKNOWN" journey.entryStationCode

  integratedBPPConfig <-
    SIBC.findIntegratedBPPConfig Nothing person.merchantOperatingCityId Spec.METRO DIBC.APPLICATION
  fareAmount <- case integratedBPPConfig.providerConfig of
    CMRLV2 cmrlCfg -> do
      let travelDatetime = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" exitTime
      fares <-
        CMRLV2GetFare.getFare
          integratedBPPConfig
          cmrlCfg
          (getId person.id)
          CMRLV2GetFare.GetFareReq
            { operatorNameId = cmrlCfg.operatorNameId,
              fromStationId = entryStation,
              toStationId = stationCode,
              ticketTypeId = cmrlCfg.ticketTypeId,
              merchantId = cmrlCfg.merchantId,
              travelDatetime = travelDatetime,
              fareTypeId = cmrlCfg.fareTypeId
            }
      pure $ case fares of
        (f : _) -> case f.categories of
          (cat : _) -> cat.offeredPrice.amount
          _ -> HighPrecMoney 0
        _ -> HighPrecMoney 0
    _ -> do
      logWarning "[SVP:Exit] No CMRLV2 config — fare defaulting to ₹0"
      pure (HighPrecMoney 0)

  logInfo $
    "[SVP:Exit] rider=" <> getId person.id
      <> " entry="
      <> entryStation
      <> " exit="
      <> stationCode
      <> " fare=₹"
      <> show fareAmount

  -- Deduct fare from wallet; gate stays closed if deduction fails
  when (fareAmount > HighPrecMoney 0) $ do
    let farePaisa = round (getHighPrecMoney fareAmount * 100) :: Int
    walletResp <-
      TWallet.walletPosting person.merchantId person.merchantOperatingCityId
        WalletPostingReq
          { customerId = getId person.id,
            postingType = REDEEM,
            operationId = getId journey.id, -- unique per journey
            pointsAmount = farePaisa
          }
    unless walletResp.success $
      throwError $
        InternalError $
          "[SVP] Wallet deduction failed for rider " <> getId person.id
            <> ": "
            <> walletResp.message

  -- Mark journey EXITED and expire tktSlNo so next QR issues a fresh serial
  QSvpJourney.updateStatusAndExitDetailsById
    DSvp.EXITED
    (Just stationCode)
    (Just exitTime)
    (Just fareAmount)
    (Just INR)
    journey.id

  Hedis.del (tktSlNoRedisKey person.id)

  logInfo $
    "[SVP:Exit] DONE rider=" <> getId person.id
      <> " "
      <> entryStation
      <> "→"
      <> stationCode
      <> " fare=₹"
      <> show fareAmount

  pure API.GateCallbackResp {allowed = True, reason = Nothing}
