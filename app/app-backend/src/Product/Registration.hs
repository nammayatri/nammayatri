module Product.Registration (auth, verify, resend, logout) where

import App.Types
import Beckn.External.Encryption (decrypt, encrypt)
import qualified Beckn.External.MyValueFirst.Flow as SF
import Beckn.Sms.Config
import qualified Beckn.Storage.Esqueleto as DB
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.APISuccess
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Common as BC
import Beckn.Types.Id
import Beckn.Utils.SlidingWindowLimiter
import Beckn.Utils.Validation (runRequestValidation)
import qualified Crypto.Number.Generate as Cryptonite
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Types.API.Registration
import Types.Error
import Utils.Auth (authTokenCacheKey)
import Utils.Common
import qualified Utils.Notifications as Notify

authHitsCountKey :: SP.Person -> Text
authHitsCountKey person = "BAP:Registration:auth" <> getId person.id <> ":hitsCount"

auth :: AuthReq -> FlowHandler AuthRes
auth req = withFlowHandlerAPI $ do
  runRequestValidation validateAuthReq req
  smsCfg <- asks (.smsCfg)
  let mobileNumber = req.mobileNumber
      countryCode = req.mobileCountryCode
  person <-
    Person.findByRoleAndMobileNumber SP.USER countryCode mobileNumber
      >>= maybe (createPerson req) return
  checkSlidingWindowLimit (authHitsCountKey person)
  let entityId = getId $ person.id
      useFakeOtpM = useFakeSms smsCfg
      scfg = sessionConfig smsCfg

  token <- makeSession scfg entityId (show <$> useFakeOtpM)
  DB.runTransaction (RegistrationToken.create token)
  whenNothing_ useFakeOtpM $ do
    otpSmsTemplate <- asks (.otpSmsTemplate)
    withLogTag ("personId_" <> getId person.id) $
      SF.sendOTP smsCfg otpSmsTemplate (countryCode <> mobileNumber) (SR.authValueHash token)
        >>= SF.checkSmsResult
  let attempts = SR.attempts token
      authId = SR.id token
  return $ AuthRes {attempts, authId}

makePerson :: EncFlow m r => AuthReq -> m SP.Person
makePerson req = do
  pid <- BC.generateGUID
  now <- getCurrentTime
  encMobNum <- encrypt req.mobileNumber
  return $
    SP.Person
      { id = pid,
        firstName = Nothing,
        middleName = Nothing,
        lastName = Nothing,
        role = SP.USER,
        gender = SP.UNKNOWN,
        identifierType = SP.MOBILENUMBER,
        email = Nothing,
        passwordHash = Nothing,
        mobileNumber = Just encMobNum,
        mobileCountryCode = Just $ req.mobileCountryCode,
        identifier = Nothing,
        rating = Nothing,
        isNew = True,
        deviceToken = Nothing,
        description = Nothing,
        createdAt = now,
        updatedAt = now
      }

makeSession ::
  MonadFlow m =>
  SmsSessionConfig ->
  Text ->
  Maybe Text ->
  m SR.RegistrationToken
makeSession SmsSessionConfig {..} entityId fakeOtp = do
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
        entityType = SR.USER,
        createdAt = now,
        updatedAt = now,
        info = Nothing
      }

generateOTPCode :: MonadFlow m => m Text
generateOTPCode =
  liftIO $ padNumber 4 <$> Cryptonite.generateBetween 1 9999

verifyHitsCountKey :: Id SP.Person -> Text
verifyHitsCountKey id = "BAP:Registration:verify:" <> getId id <> ":hitsCount"

verify :: Id SR.RegistrationToken -> AuthVerifyReq -> FlowHandler AuthVerifyRes
verify tokenId req = withFlowHandlerAPI $ do
  runRequestValidation validateAuthVerifyReq req
  regToken@SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
  checkSlidingWindowLimit (verifyHitsCountKey $ Id entityId)
  when verified $ throwError $ AuthBlocked "Already verified."
  checkForExpiry authExpiry updatedAt
  unless (authValueHash == req.otp) $ throwError InvalidAuthData
  person <- checkPersonExists entityId
  let isNewPerson = person.isNew
  let deviceToken = Just req.deviceToken
  cleanCachedTokens person.id
  DB.runTransaction $ do
    RegistrationToken.deleteByPersonIdExceptNew person.id tokenId
    RegistrationToken.setVerified tokenId
    Person.updateDeviceToken person.id deviceToken
    when isNewPerson $
      Person.setIsNewFalse person.id
  when isNewPerson $
    Notify.notifyOnRegistration regToken person.id deviceToken
  updPerson <- Person.findById (Id entityId) >>= fromMaybeM (PersonDoesNotExist entityId)
  decPerson <- decrypt updPerson
  let personAPIEntity = SP.makePersonAPIEntity decPerson
  return $ AuthVerifyRes token personAPIEntity
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
        throwError TokenExpired

getRegistrationTokenE :: EsqDBFlow m r => Id SR.RegistrationToken -> m SR.RegistrationToken
getRegistrationTokenE tokenId =
  RegistrationToken.findById tokenId >>= fromMaybeM (TokenNotFound $ getId tokenId)

createPerson :: (EncFlow m r, EsqDBFlow m r) => AuthReq -> m SP.Person
createPerson req = do
  person <- makePerson req
  DB.runTransaction $ Person.create person
  pure person

checkPersonExists :: EsqDBFlow m r => Text -> m SP.Person
checkPersonExists entityId =
  Person.findById (Id entityId) >>= fromMaybeM (PersonDoesNotExist entityId)

resend :: Id SR.RegistrationToken -> FlowHandler ResendAuthRes
resend tokenId = withFlowHandlerAPI $ do
  SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
  person <- checkPersonExists entityId
  unless (attempts > 0) $ throwError $ AuthBlocked "Attempts limit exceed."
  smsCfg <- smsCfg <$> ask
  otpSmsTemplate <- otpSmsTemplate <$> ask
  mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  withLogTag ("personId_" <> entityId) $
    SF.sendOTP smsCfg otpSmsTemplate (countryCode <> mobileNumber) authValueHash
      >>= SF.checkSmsResult
  DB.runTransaction $ RegistrationToken.updateAttempts (attempts - 1) id
  return $ AuthRes tokenId (attempts - 1)

cleanCachedTokens :: EsqDBFlow m r => Id SP.Person -> m ()
cleanCachedTokens personId = do
  regTokens <- RegistrationToken.findAllByPersonId personId
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.deleteKeyRedis key

logout :: Id SP.Person -> FlowHandler APISuccess
logout personId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  cleanCachedTokens personId
  DB.runTransaction $ do
    Person.updateDeviceToken personId Nothing
    RegistrationToken.deleteByPersonId personId
  pure Success
