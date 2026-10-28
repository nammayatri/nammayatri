module Domain.Action.UI.SocialLogin where

import qualified API.Types.UI.SocialLogin as SL
import Data.Aeson
import qualified Data.Text as T
import qualified Domain.Action.UI.Registration as PR
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (DbHash, decrypt, getDbHash)
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimit)
import Lib.ConfigPilot.Interface.Types (getConfig)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import qualified SharedLogic.MerchantConfig as SMC
import qualified SharedLogic.OTP as SOTP
import qualified SharedLogic.SocialLogin as SSL
import qualified Storage.CachedQueries.Merchant as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMerchantOpCity
import Storage.ConfigPilot.Config.MerchantConfig (MerchantConfigDimensions (..))
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Person as PQ
import qualified Storage.Queries.PersonSocialIdentity as QPSI
import qualified Storage.Queries.RegistrationToken as QR
import Tools.Error

googleTokenInfoUrl :: Text -> String -- TODO: change this to local validation as mentioned in this doc: https://developers.google.com/identity/sign-in/web/backend-auth#verify-the-integrity-of-the-id-token
googleTokenInfoUrl token = "https://oauth2.googleapis.com/tokeninfo?id_token=" <> T.unpack token

data TokenInfo = TokenInfo
  { email :: Text,
    email_verified :: Maybe String,
    name :: Maybe Text,
    picture :: Maybe Text
  }
  deriving (Show)

instance FromJSON TokenInfo where
  parseJSON = withObject "TokenInfo" $ \v ->
    TokenInfo
      <$> v .: "email"
      <*> v .:? "email_verified"
      <*> v .:? "name"
      <*> v .:? "picture"

fetchTokenInfo :: Text -> SL.OAuthProvider -> Text -> IO (Either String TokenInfo)
fetchTokenInfo iosValidateEnpoint oauthProvider token = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest $ case oauthProvider of
    SL.Google -> googleTokenInfoUrl token
    SL.IOS -> T.unpack $ iosValidateEnpoint <> token
  response <- httpLbs request manager
  let statusCode' = statusCode $ responseStatus response
  if statusCode' == 200
    then return $ eitherDecode $ responseBody response
    else return $ Left $ "Failed to fetch token info, status code: " ++ show statusCode'

postSocialLogin :: SL.SocialLoginReq -> Environment.Flow SL.SocialLoginRes
postSocialLogin req = do
  iosValidateEnpoint <- asks (.iosValidateEnpoint)
  merchant <- CQMOC.findByShortId req.merchantShortId >>= fromMaybeM (MerchantDoesNotExist req.merchantShortId.getShortId)
  result <- L.runIO $ fetchTokenInfo iosValidateEnpoint req.oauthProvider req.tokenId
  case result of
    Right info -> do
      oldPerson <- PQ.findByEmailAndMerchantId merchant.id info.email
      (person, isNew) <-
        case oldPerson of
          Just person' -> pure (person', False)
          Nothing ->
            (,True) <$> do
              let authReq = buildAuthReq info.email
              cloudType <- asks (.cloudType)
              PR.createPerson authReq SP.EMAIL Nothing Nothing Nothing Nothing Nothing Nothing cloudType merchant Nothing Nothing
      QR.deleteByPersonId person.id
      token <- makeSession person.id.getId merchant.id.getId person.merchantOperatingCityId.getId
      _ <- QR.create token
      pure $ SL.SocialLoginRes isNew token.token
    Left _ -> throwError . InternalError $ show req.oauthProvider <> ", idToken: " <> req.tokenId <> " error: "
  where
    buildAuthReq email =
      PR.AuthReq
        { mobileNumber = Nothing,
          mobileCountryCode = Nothing,
          identifierType = Just SP.EMAIL,
          merchantId = req.merchantShortId,
          deviceToken = Nothing,
          notificationToken = Nothing,
          whatsappNotificationEnroll = Nothing,
          firstName = req.name,
          middleName = Nothing,
          lastName = Nothing,
          email = Just email,
          language = Nothing,
          businessEmail = Nothing,
          gender = Nothing,
          otpChannel = Nothing,
          registrationLat = req.registrationLat,
          registrationLon = req.registrationLon,
          enableOtpLessRide = req.enableOtpLessRide,
          allowBlockedUserLogin = Nothing,
          isOperatorReq = Nothing,
          reuseToken = Nothing,
          operatorBadgeToken = Nothing,
          deviceSerialNumber = Nothing,
          vehicleType = Nothing
        }

makeSession ::
  Text ->
  Text ->
  Text ->
  Environment.Flow SR.RegistrationToken
makeSession entityId merchantId merchantOperatingCityId = do
  otp <- generateOTPCode
  rtid <- L.generateGUID
  token <- L.generateGUID
  now <- getCurrentTime
  return $
    SR.RegistrationToken
      { id = Id rtid,
        token = token,
        attempts = 3,
        authMedium = SR.EMAIL,
        authType = SR.OAUTH,
        authValueHash = otp,
        verified = True,
        authExpiry = 3,
        tokenExpiry = 356,
        entityId = entityId,
        merchantId = merchantId,
        merchantOperatingCityId = merchantOperatingCityId,
        entityType = SR.USER,
        createdAt = now,
        updatedAt = now,
        info = Nothing,
        createdViaPartnerOrgId = Nothing
      }

postSocialUpdateProfile ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    SL.SocialUpdateProfileReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postSocialUpdateProfile (mbPersonId, _) req = do
  personId <- maybe (throwError $ InternalError "Not Implemented for dashboard") pure mbPersonId
  person <- PQ.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  whenJust req.mobileNumber $ \mobileNumber -> do
    mobileNumberHash <- getDbHash mobileNumber
    unless ((person.mobileNumber <&> (.hash)) == Just mobileNumberHash) $
      throwError $ InvalidRequest "Mobile number can only be changed through OTP verification"
  let updatedPerson =
        person
          { SP.firstName = req.firstName <|> person.firstName,
            SP.lastName = req.lastName <|> person.lastName
          }
  PQ.updateByPrimaryKey updatedPerson
  pure Kernel.Types.APISuccess.Success

postSocialLoginV2 :: SL.SocialLoginV2Req -> Environment.Flow SL.SocialLoginV2Res
postSocialLoginV2 req = do
  merchant <- CQMOC.findByShortId req.merchantShortId >>= fromMaybeM (MerchantDoesNotExist req.merchantShortId.getShortId)
  verifiedLogin <- SSL.verifySocialToken req.oauthProvider req.nonce req.authorizationCode req.tokenId
  now <- getCurrentTime
  mbIdentity <- SSL.findIdentity merchant.id verifiedLogin
  case mbIdentity of
    Just socialIdentity | socialIdentity.validTill > now -> do
      person <- PQ.findById socialIdentity.personId >>= fromMaybeM (PersonDoesNotExist socialIdentity.personId.getId)
      SSL.recordLogin socialIdentity verifiedLogin
      loggedIn person
    _ -> do
      mbPerson <- if isJust mbIdentity then pure Nothing else SSL.findAccountByTrustedEmail merchant.id (SSL.trustedEmail verifiedLogin)
      case mbPerson of
        Just person -> do
          void $ SSL.linkIdentity person verifiedLogin
          loggedIn person
        Nothing -> linkRequired <$> SSL.storePendingLink merchant.id verifiedLogin

loggedIn :: SP.Person -> Environment.Flow SL.SocialLoginV2Res
loggedIn person = do
  PR.cleanCachedTokens person.id
  QR.deleteByPersonId person.id
  token <- makeOAuthSession person.id.getId person.merchantId.getId person.merchantOperatingCityId.getId
  QR.create token
  pure SL.SocialLoginV2Res {status = SL.LOGGED_IN, token = Just token.token, linkToken = Nothing}

linkRequired :: Text -> SL.SocialLoginV2Res
linkRequired linkToken = SL.SocialLoginV2Res {status = SL.LINK_REQUIRED, token = Nothing, linkToken = Just linkToken}

makeOAuthSession :: Text -> Text -> Text -> Environment.Flow SR.RegistrationToken
makeOAuthSession entityId merchantId merchantOperatingCityId = do
  sessionCfg <- asks (.smsCfg.sessionConfig)
  session <- makeSession entityId merchantId merchantOperatingCityId
  pure session {SR.attempts = sessionCfg.attempts, SR.authExpiry = sessionCfg.authExpiry, SR.tokenExpiry = sessionCfg.tokenExpiry}

data PendingEmailAuth = PendingEmailAuth
  { pendingMerchantId :: Id Domain.Types.Merchant.Merchant,
    pendingEmail :: Text,
    pendingOtpHash :: DbHash,
    pendingMaxAttempts :: Int
  }
  deriving (Generic, ToJSON, FromJSON)

pendingEmailAuthKey :: Text -> Text
pendingEmailAuthKey authId = "SocialLogin:PendingEmailAuth:" <> authId

pendingEmailAuthAttemptsKey :: Text -> Text
pendingEmailAuthAttemptsKey authId = "SocialLogin:PendingEmailAuthAttempts:" <> authId

postAuthEmailV2 :: Maybe Text -> SL.EmailAuthV2Req -> Environment.Flow SL.EmailAuthV2Res
postAuthEmailV2 mbXForwardedFor req = do
  unless (SSL.isWellFormedEmail req.email) $ throwError InvalidEmailAddress
  merchant <- CQMOC.findByShortId req.merchantShortId >>= fromMaybeM (MerchantDoesNotExist req.merchantShortId.getShortId)
  emailHash <- getDbHash (SSL.normalizeEmail req.email)
  checkSlidingWindowLimit ("SocialLogin:EmailAuthHits:" <> merchant.id.getId <> ":" <> show emailHash)
  SSL.findAccountByTrustedEmail merchant.id (Just req.email) >>= \case
    Just person -> do
      storedEmail <- maybe (pure req.email) decrypt person.email
      authRes <- PR.auth (emailAuthReq req storedEmail) Nothing Nothing Nothing Nothing Nothing mbXForwardedFor Nothing Nothing
      pure SL.EmailAuthV2Res {authId = authRes.authId.getId, attempts = authRes.attempts}
    Nothing -> do
      mbClientIP <- PR.extractClientIP mbXForwardedFor
      whenJust mbClientIP $ \clientIP -> do
        ipBlocked <- SMC.isIPBlocked clientIP
        when ipBlocked $ throwError IpHitsLimitExceeded
      merchantOpCity <- CQMerchantOpCity.findByMerchantIdAndCity merchant.id merchant.defaultCity >>= fromMaybeM (MerchantOperatingCityNotFound $ show merchant.defaultCity)
      riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOpCity.id.getId)
      merchantConfigs <- getConfig (MerchantConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) Nothing
      whenJust mbClientIP $ \clientIP -> fork "Email Auth IP Fraud Check" $ do
        SMC.updateCustomerAuthCountersByIP clientIP merchantConfigs
        (isFraudDetected, mbMerchantConfigId) <- SMC.checkAuthFraudByIP merchantConfigs clientIP
        when isFraudDetected $
          whenJust mbMerchantConfigId $ \mcId ->
            SMC.blockCustomerByIP clientIP (Just mcId) riderConfig.blockedUntilInMins
      unless (req.email `elem` merchant.fakeOtpEmails) $
        SSL.enforceEmailOtpLimit merchantConfigs req.email
      smsCfg <- asks (.smsCfg)
      let sessionCfg = smsCfg.sessionConfig
      authId <- generateGUID
      let mbFakeOtp = if req.email `elem` merchant.fakeOtpEmails then Just "7891" else show <$> smsCfg.useFakeSms
      otp <- maybe generateOTPCode pure mbFakeOtp
      otpHash <- getDbHash otp
      Redis.setExp (pendingEmailAuthKey authId) PendingEmailAuth {pendingMerchantId = merchant.id, pendingEmail = req.email, pendingOtpHash = otpHash, pendingMaxAttempts = sessionCfg.attempts} (sessionCfg.authExpiry * 60)
      when (isNothing mbFakeOtp) $
        SOTP.sendOTP SOTP.EMAIL otp (Id authId) merchant.id merchantOpCity.id Nothing Nothing (Just req.email) riderConfig.emailOtpConfig Nothing
      pure SL.EmailAuthV2Res {authId, attempts = sessionCfg.attempts}

emailAuthReq :: SL.EmailAuthV2Req -> Text -> PR.AuthReq
emailAuthReq req authEmail =
  PR.AuthReq
    { mobileNumber = Nothing,
      mobileCountryCode = Nothing,
      identifierType = Just SP.EMAIL,
      merchantId = req.merchantShortId,
      deviceToken = Nothing,
      notificationToken = Nothing,
      whatsappNotificationEnroll = Nothing,
      firstName = Nothing,
      middleName = Nothing,
      lastName = Nothing,
      email = Just authEmail,
      language = Nothing,
      businessEmail = Nothing,
      gender = Nothing,
      otpChannel = Nothing,
      registrationLat = Nothing,
      registrationLon = Nothing,
      enableOtpLessRide = Nothing,
      allowBlockedUserLogin = Just True,
      isOperatorReq = Nothing,
      reuseToken = Nothing,
      operatorBadgeToken = Nothing,
      deviceSerialNumber = Nothing,
      vehicleType = Nothing
    }

postAuthEmailVerifyV2 :: Text -> SL.EmailAuthVerifyV2Req -> Environment.Flow SL.SocialLoginV2Res
postAuthEmailVerifyV2 authId req =
  Redis.safeGet (pendingEmailAuthKey authId) >>= \case
    Just (pending :: PendingEmailAuth) -> do
      sessionCfg <- asks (.smsCfg.sessionConfig)
      attemptsUsed <- Redis.incr (pendingEmailAuthAttemptsKey authId)
      when (attemptsUsed == 1) $ Redis.expire (pendingEmailAuthAttemptsKey authId) (sessionCfg.authExpiry * 60)
      when (attemptsUsed > fromIntegral pending.pendingMaxAttempts) $ throwError (AuthBlocked "Too many incorrect attempts")
      otpHash <- getDbHash req.otp
      unless (otpHash == pending.pendingOtpHash) $ throwError InvalidAuthData
      Redis.del (pendingEmailAuthKey authId)
      linkRequired <$> SSL.storePendingEmailLink pending.pendingMerchantId pending.pendingEmail
    Nothing -> do
      verifyRes <-
        PR.verify
          (Id authId)
          PR.AuthVerifyReq
            { otp = req.otp,
              deviceToken = req.deviceToken,
              whatsappNotificationEnroll = Nothing,
              userPasswordString = Nothing,
              socialLinkToken = Nothing
            }
          Nothing
          Nothing
      pure SL.SocialLoginV2Res {status = SL.LOGGED_IN, token = Just verifyRes.token, linkToken = Nothing}

postSocialLink ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    SL.SocialLinkReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postSocialLink (mbPersonId, _) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- PQ.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  verifiedLogin <- SSL.verifySocialToken req.oauthProvider req.nonce req.authorizationCode req.tokenId
  void $ SSL.linkIdentity person verifiedLogin
  pure Kernel.Types.APISuccess.Success

postSocialUnlink ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    SL.SocialUnlinkReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postSocialUnlink (mbPersonId, _) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  SSL.unlinkProvider personId (SSL.toDomainProvider req.oauthProvider)
  pure Kernel.Types.APISuccess.Success

getSocialLinkedAccounts ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow SL.LinkedSocialAccountsRes
  )
getSocialLinkedAccounts (mbPersonId, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  identities <- QPSI.findAllByPersonId personId
  accounts <-
    forM identities $ \socialIdentity -> do
      email <- mapM decrypt socialIdentity.email
      pure SL.LinkedSocialAccount {oauthProvider = SSL.toApiProvider socialIdentity.provider, email, isPrivateEmail = socialIdentity.isPrivateEmail, validTill = socialIdentity.validTill}
  pure SL.LinkedSocialAccountsRes {accounts}
