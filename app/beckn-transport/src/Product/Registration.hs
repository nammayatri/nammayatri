module Product.Registration (checkPersonExists, auth, verify, resend, logout) where

import App.Types
import Beckn.External.Encryption
import qualified Beckn.External.MyValueFirst.Flow as SF
import Beckn.Sms.Config
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.APISuccess
import Beckn.Types.Common as BC
import Beckn.Types.Id
import Beckn.Utils.SlidingWindowLimiter
import Beckn.Utils.Validation (runRequestValidation)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Types.API.Registration
import Types.Error
import qualified Types.Storage.Person as SP
import qualified Types.Storage.RegistrationToken as SR
import Utils.Auth (authTokenCacheKey)
import Utils.Common
import qualified Utils.Notifications as Notify

authHitsCountKey :: SP.Person -> Text
authHitsCountKey person = "BPP:Registration:auth" <> getId person.id <> ":hitsCount"

auth :: AuthReq -> FlowHandler AuthRes
auth req = withFlowHandlerAPI $ do
  runRequestValidation validateInitiateLoginReq req
  smsCfg <- asks (.smsCfg)
  let mobileNumber = req.mobileNumber
      countryCode = req.mobileCountryCode
  person <- QP.findByMobileNumber countryCode mobileNumber >>= fromMaybeM PersonDoesNotExist
  checkSlidingWindowLimit (authHitsCountKey person)
  let entityId = getId $ person.id
      useFakeOtpM = useFakeSms smsCfg
      scfg = sessionConfig smsCfg
  regToken <- case useFakeOtpM of
    Just _ -> do
      token <- makeSession scfg entityId SR.USER (show <$> useFakeOtpM)
      QR.create token
      return token
    Nothing -> do
      token <- makeSession scfg entityId SR.USER Nothing
      QR.create token
      otpSmsTemplate <- asks (.otpSmsTemplate)
      withLogTag ("personId_" <> getId person.id) $
        SF.sendOTP smsCfg otpSmsTemplate (countryCode <> mobileNumber) (SR.authValueHash token)
          >>= SF.checkRegistrationSmsResult
      return token
  let attempts = SR.attempts regToken
      authId = SR.id regToken
  return $ AuthRes {attempts, authId}

makeSession ::
  MonadFlow m =>
  SmsSessionConfig ->
  Text ->
  SR.RTEntityType ->
  Maybe Text ->
  m SR.RegistrationToken
makeSession SmsSessionConfig {..} entityId entityType fakeOtp = do
  otp <- maybe generateOTPCode return fakeOtp
  rtid <- L.generateGUID
  token <- L.generateGUID
  now <- getCurrentTime
  return $
    SR.RegistrationToken
      { id = Id rtid,
        token = token,
        attempts = attempts,
        authMedium = SR.SMS,
        authType = SR.OTP,
        authValueHash = otp,
        verified = False,
        authExpiry = authExpiry,
        tokenExpiry = tokenExpiry,
        entityId = entityId,
        entityType = entityType,
        createdAt = now,
        updatedAt = now,
        info = Nothing
      }

verifyHitsCountKey :: Id SP.Person -> Text
verifyHitsCountKey id = "BPP:Registration:verify:" <> getId id <> ":hitsCount"

verify :: Id SR.RegistrationToken -> AuthVerifyReq -> FlowHandler AuthVerifyRes
verify tokenId req = withFlowHandlerAPI $ do
  runRequestValidation validateAuthVerifyReq req
  regToken@SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId
  checkSlidingWindowLimit (verifyHitsCountKey $ Id entityId)
  when verified $ throwError $ AuthBlocked "Already verified."
  checkForExpiry authExpiry updatedAt
  unless (authValueHash == req.otp) $ throwError InvalidAuthData
  person <- checkPersonExists entityId
  let isNewPerson = person.isNew
  clearOldRegToken person tokenId
  let deviceToken = Just req.deviceToken
  DB.runSqlDBTransaction $ do
    QR.setVerified tokenId
    QP.updateDeviceToken person.id deviceToken
    when isNewPerson $
      QP.setIsNewFalse person.id
  when isNewPerson $
    Notify.notifyOnRegistration regToken person.id deviceToken
  updPers <- QP.findPersonById (Id entityId) >>= fromMaybeM PersonNotFound
  decPerson <- decrypt updPers
  personAPIEntity <- SP.buildPersonAPIEntity decPerson
  return $ AuthVerifyRes token personAPIEntity
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
        throwError TokenExpired

checkRegistrationTokenExists :: DBFlow m r => Id SR.RegistrationToken -> m SR.RegistrationToken
checkRegistrationTokenExists tokenId =
  QR.findRegistrationToken tokenId >>= fromMaybeM (TokenNotFound $ getId tokenId)

checkPersonExists :: DBFlow m r => Text -> m SP.Person
checkPersonExists entityId =
  QP.findPersonById (Id entityId) >>= fromMaybeM PersonNotFound

resend :: Id SR.RegistrationToken -> FlowHandler ResendAuthRes
resend tokenId = withFlowHandlerAPI $ do
  SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId
  person <- checkPersonExists entityId
  unless (attempts > 0) $ throwError $ AuthBlocked "Attempts limit exceed."
  smsCfg <- smsCfg <$> ask
  otpSmsTemplate <- otpSmsTemplate <$> ask
  mobileNumber <- decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  withLogTag ("personId_" <> entityId) $
    SF.sendOTP smsCfg otpSmsTemplate (countryCode <> mobileNumber) authValueHash
      >>= SF.checkRegistrationSmsResult
  _ <- QR.updateAttempts (attempts - 1) id
  return $ AuthRes tokenId (attempts - 1)

clearOldRegToken :: DBFlow m r => SP.Person -> Id SR.RegistrationToken -> m ()
clearOldRegToken person = QR.deleteByPersonIdExceptNew (getId $ person.id)

logout :: Id SP.Person -> FlowHandler APISuccess
logout personId = withFlowHandlerAPI $ do
  regTokens <- QR.findAllByPersonId personId
  -- We should have only one RegToken at this point, but just in case we use findAll
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.deleteKeyRedis key
  uperson <-
    QP.findPersonById personId
      >>= fromMaybeM PersonNotFound
  DB.runSqlDBTransaction $ do
    QP.updateDeviceToken uperson.id Nothing
    QR.deleteByPersonId $ getId personId
  pure Success
