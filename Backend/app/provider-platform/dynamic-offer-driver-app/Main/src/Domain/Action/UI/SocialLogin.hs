module Domain.Action.UI.SocialLogin where

import qualified API.Types.UI.SocialLogin
import qualified API.Types.UI.SocialLogin as SL
import Data.Aeson
import qualified Data.Text as T
import qualified Domain.Action.UI.Registration as DR
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
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
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import qualified SharedLogic.OTP as SOTP
import qualified SharedLogic.SocialLogin as SSL
import qualified Storage.CachedQueries.Merchant as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
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
  merchant <- case req.merchantShortId of
    Just merchantShortId -> CQMOC.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
    Nothing -> throwError $ InvalidRequest "Cannot perform social auth without MerchantShortId"
  result <- L.runIO $ fetchTokenInfo iosValidateEnpoint req.oauthProvider req.tokenId
  case result of
    Right info -> do
      oldPerson <- PQ.findByEmailAndMerchantIdAndRole (Just $ info.email) merchant.id SP.DRIVER
      moc <- CQMOC.findByMerchantIdAndCity merchant.id req.merchantOperatingCity >>= fromMaybeM (MerchantOperatingCityNotFound $ show req.merchantOperatingCity)
      (person, isNew) <-
        case oldPerson of
          Just person' -> pure (person', False)
          Nothing ->
            (,True) <$> do
              deploymentVersion <- asks (.version)
              cloudType <- asks (.cloudType)
              let createPersonInput = buildCreatePersonInput moc.city req.name info.email merchant
              DR.createDriverWithDetails createPersonInput Nothing Nothing Nothing Nothing Nothing (Just deploymentVersion.getDeploymentVersion) cloudType merchant.id moc.id False Nothing Nothing Nothing Nothing
      QR.deleteByPersonId (getId person.id)
      token <- makeSession person.id.getId merchant.id.getId moc.id.getId
      _ <- QR.create token
      pure $ SL.SocialLoginRes isNew token.token
    Left _ -> throwError . FailedToVerifyIdToken $ show req.oauthProvider <> ", idToken: " <> req.tokenId <> " error: "
  where
    buildCreatePersonInput city name email merchant =
      DR.AuthReq
        { mobileNumber = Nothing,
          mobileCountryCode = Nothing,
          name = name,
          merchantId = merchant.id.getId,
          merchantOperatingCity = Just city,
          email = Just email,
          identifierType = Just SP.EMAIL,
          registrationLat = req.registrationLat,
          registrationLon = req.registrationLon,
          otpChannel = Nothing,
          password = Nothing,
          employeeId = Nothing,
          isOperatorReq = Nothing
        }

makeSession ::
  Text ->
  Text ->
  Text ->
  Environment.Flow SR.RegistrationToken
makeSession entityId merchantId merchantOpCityId = do
  otp <- generateOTPCode
  rtid <- generateGUID
  token <- generateGUID
  now <- getCurrentTime
  return $
    SR.RegistrationToken
      { id = Id rtid,
        token = token,
        attempts = 3, -- TODO: maybe change later
        authMedium = SR.EMAIL,
        authType = SR.OAUTH,
        authValueHash = otp,
        verified = True,
        authExpiry = 3,
        tokenExpiry = 365,
        entityId = entityId,
        merchantId = merchantId,
        merchantOperatingCityId = merchantOpCityId,
        entityType = SR.USER,
        createdAt = now,
        updatedAt = now,
        info = Nothing,
        alternateNumberAttempts = 3 -- TODO: change later
      }

postSocialUpdateProfile ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.SocialLogin.SocialUpdateProfileReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postSocialUpdateProfile (mbPersonId, merchantId, _) req = do
  personId <- maybe (throwError $ InternalError "Not Implemented for dashboard") pure mbPersonId
  person <- PQ.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  whenJust req.mobileNumber $ \mobileNumber -> do
    mobileNumberHash <- getDbHash mobileNumber
    unless ((person.mobileNumber <&> (.hash)) == Just mobileNumberHash) $
      throwError $ InvalidRequest "Mobile number can only be changed through OTP verification"
  PQ.findByEmailAndMerchantIdAndRole (Just req.email) merchantId SP.DRIVER >>= \case
    Just existingPerson
      | personId /= existingPerson.id ->
        throwError $ DriverEmailAlreadyExists (show req.email)
    _ -> return ()
  let emailChanged = (SSL.normalizeEmail <$> person.email) /= Just (SSL.normalizeEmail req.email)
  when (emailChanged && isJust person.email && SSL.hasTrustedEmail person) $
    throwError $ InvalidRequest "Email can only be changed through OTP verification"
  let updatedPerson =
        person
          { SP.firstName = fromMaybe person.firstName req.firstName,
            SP.lastName = req.lastName <|> person.lastName,
            SP.email = Just req.email,
            SP.emailVerified = if emailChanged then Just False else person.emailVerified,
            SP.emailSource = if emailChanged then Nothing else person.emailSource
          }
  PQ.updatePersonDetails updatedPerson
  when emailChanged $ SSL.unlinkIdentitiesWithOtherEmail personId (Just req.email)
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
      mbPerson <- if isJust mbIdentity then pure Nothing else SSL.findAccountByTrustedEmail merchant.id SP.DRIVER (SSL.trustedEmail verifiedLogin)
      case mbPerson of
        Just person -> do
          void $ SSL.linkIdentity person verifiedLogin
          loggedIn person
        Nothing -> linkRequired <$> SSL.storePendingLink merchant.id verifiedLogin

loggedIn :: SP.Person -> Environment.Flow SL.SocialLoginV2Res
loggedIn person = do
  DR.cleanCachedTokens person.id
  QR.deleteByPersonId (getId person.id)
  token <- makeOAuthSession person.id.getId person.merchantId.getId person.merchantOperatingCityId.getId
  QR.create token
  pure SL.SocialLoginV2Res {status = SL.LOGGED_IN, token = Just token.token, linkToken = Nothing}

linkRequired :: Text -> SL.SocialLoginV2Res
linkRequired linkToken = SL.SocialLoginV2Res {status = SL.LINK_REQUIRED, token = Nothing, linkToken = Just linkToken}

makeOAuthSession :: Text -> Text -> Text -> Environment.Flow SR.RegistrationToken
makeOAuthSession entityId merchantId merchantOpCityId = do
  sessionCfg <- asks (.smsCfg.sessionConfig)
  session <- makeSession entityId merchantId merchantOpCityId
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
  unless (SSL.isWellFormedEmail req.email) $ throwError (InvalidRequest "Invalid email address")
  merchant <- CQMOC.findByShortId req.merchantShortId >>= fromMaybeM (MerchantDoesNotExist req.merchantShortId.getShortId)
  emailHash <- getDbHash (SSL.normalizeEmail req.email)
  checkSlidingWindowLimit ("SocialLogin:EmailAuthHits:" <> merchant.id.getId <> ":" <> show emailHash)
  SSL.findAccountByTrustedEmail merchant.id SP.DRIVER (Just req.email) >>= \case
    Just person -> do
      authRes <- DR.auth False (emailAuthReq merchant req (fromMaybe req.email person.email)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing mbXForwardedFor
      pure SL.EmailAuthV2Res {authId = authRes.authId.getId, attempts = authRes.attempts}
    Nothing -> do
      merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id req.merchantOperatingCity >>= fromMaybeM (MerchantOperatingCityNotFound $ show req.merchantOperatingCity)
      mbClientIP <- DR.extractClientIP mbXForwardedFor
      whenJust mbClientIP $ \clientIP -> do
        ipBlocked <- DR.isIPBlocked merchantOpCity.id.getId clientIP
        when ipBlocked $ throwError IpHitsLimitExceeded
      transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
      whenJust mbClientIP $ \clientIP ->
        fork "Driver Email Auth IP Fraud Check" $ DR.checkAndUpdateAuthFraudByIP merchantOpCity.id.getId clientIP transporterConfig
      unless (req.email `elem` transporterConfig.fakeOtpEmails) $
        SSL.enforceEmailOtpLimit req.email transporterConfig
      smsCfg <- asks (.smsCfg)
      let sessionCfg = smsCfg.sessionConfig
      authId <- generateGUID
      let mbFakeOtp = if req.email `elem` transporterConfig.fakeOtpEmails then Just "7891" else show <$> smsCfg.useFakeSms
      otp <- maybe generateOTPCode pure mbFakeOtp
      otpHash <- getDbHash otp
      Redis.setExp (pendingEmailAuthKey authId) PendingEmailAuth {pendingMerchantId = merchant.id, pendingEmail = req.email, pendingOtpHash = otpHash, pendingMaxAttempts = sessionCfg.attempts} (sessionCfg.authExpiry * 60)
      when (isNothing mbFakeOtp) $
        SOTP.sendOTP SOTP.EMAIL otp (Id authId) merchant.id merchantOpCity.id Nothing Nothing (Just req.email) Nothing
      pure SL.EmailAuthV2Res {authId, attempts = sessionCfg.attempts}

emailAuthReq :: Domain.Types.Merchant.Merchant -> SL.EmailAuthV2Req -> Text -> DR.AuthReq
emailAuthReq merchant req authEmail =
  DR.AuthReq
    { mobileNumber = Nothing,
      mobileCountryCode = Nothing,
      name = Nothing,
      merchantId = merchant.id.getId,
      merchantOperatingCity = Just req.merchantOperatingCity,
      email = Just authEmail,
      identifierType = Just SP.EMAIL,
      registrationLat = Nothing,
      registrationLon = Nothing,
      otpChannel = Nothing,
      password = Nothing,
      employeeId = Nothing,
      isOperatorReq = Nothing
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
        DR.verify
          (Id authId)
          DR.AuthVerifyReq
            { otp = req.otp,
              deviceToken = req.deviceToken,
              whatsappNotificationEnroll = Nothing,
              socialLinkToken = Nothing
            }
          Nothing
      pure SL.SocialLoginV2Res {status = SL.LOGGED_IN, token = Just verifyRes.token, linkToken = Nothing}

type ProviderAuth =
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  )

postSocialLink :: ProviderAuth -> SL.SocialLinkReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postSocialLink (mbPersonId, _, _) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- PQ.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  verifiedLogin <- SSL.verifySocialToken req.oauthProvider req.nonce req.authorizationCode req.tokenId
  void $ SSL.linkIdentity person verifiedLogin
  pure Kernel.Types.APISuccess.Success

postSocialUnlink :: ProviderAuth -> SL.SocialUnlinkReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postSocialUnlink (mbPersonId, _, _) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  SSL.unlinkProvider personId (SSL.toDomainProvider req.oauthProvider)
  pure Kernel.Types.APISuccess.Success

getSocialLinkedAccounts :: ProviderAuth -> Environment.Flow SL.LinkedSocialAccountsRes
getSocialLinkedAccounts (mbPersonId, _, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  identities <- QPSI.findAllByPersonId personId
  accounts <-
    forM identities $ \socialIdentity -> do
      email <- mapM decrypt socialIdentity.email
      pure SL.LinkedSocialAccount {oauthProvider = SSL.toApiProvider socialIdentity.provider, email, isPrivateEmail = socialIdentity.isPrivateEmail, validTill = socialIdentity.validTill}
  pure SL.LinkedSocialAccountsRes {accounts}
