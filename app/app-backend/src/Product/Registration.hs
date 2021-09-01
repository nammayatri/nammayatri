module Product.Registration (auth, verify, resend, logout) where

import App.Types
import Beckn.External.Encryption (decrypt, encrypt)
import qualified Beckn.External.MyValueFirst.Flow as SF
import Beckn.Sms.Config
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.APISuccess
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Common as BC
import Beckn.Types.Id
import Beckn.Utils.SlidingWindowLimiter
import Beckn.Utils.Validation (runRequestValidation)
import qualified Crypto.Number.Generate as Cryptonite
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Types.API.Registration
import Types.Error
import qualified Types.Storage.Person as SP
import qualified Types.Storage.RegistrationToken as SR
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
  regToken <- case useFakeOtpM of
    Just _ -> do
      token <- makeSession scfg entityId (show <$> useFakeOtpM)
      DB.runSqlDB (RegistrationToken.create token)
      return token
    Nothing -> do
      token <- makeSession scfg entityId Nothing
      DB.runSqlDB (RegistrationToken.create token)
      otpSmsTemplate <- asks (.otpSmsTemplate)
      withLogTag ("personId_" <> getId person.id) $
        SF.sendOTP smsCfg otpSmsTemplate (countryCode <> mobileNumber) (SR.authValueHash token)
          >>= SF.checkRegistrationSmsResult
      return token
  let attempts = SR.attempts regToken
      authId = SR.id regToken
  return $ AuthRes {attempts, authId}

makePerson :: EncFlow m r => AuthReq -> m SP.Person
makePerson req = do
  pid <- BC.generateGUID
  now <- getCurrentTime
  encMobNum <- encrypt $ Just req.mobileNumber
  return $
    SP.Person
      { id = pid,
        firstName = Nothing,
        middleName = Nothing,
        lastName = Nothing,
        fullName = Nothing,
        role = SP.USER,
        gender = SP.UNKNOWN,
        identifierType = SP.MOBILENUMBER,
        email = Nothing,
        passwordHash = Nothing,
        mobileNumber = encMobNum,
        mobileCountryCode = Just $ req.mobileCountryCode,
        identifier = Nothing,
        rating = Nothing,
        isNew = True,
        deviceToken = Nothing,
        udf1 = Nothing,
        udf2 = Nothing,
        organizationId = Nothing,
        description = Nothing,
        createdAt = now,
        updatedAt = now
      }

makeSession ::
  DBFlow m r =>
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
      { id = rtid,
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
  L.runIO $ padNumber 4 <$> Cryptonite.generateBetween 1 9999

verifyHitsCountKey :: Id SP.Person -> Text
verifyHitsCountKey id = "BAP:Registration:verify:" <> getId id <> ":hitsCount"

verify :: Text -> AuthVerifyReq -> FlowHandler AuthVerifyRes
verify tokenId req =
  withFlowHandlerAPI $ do
    runRequestValidation validateAuthVerifyReq req
    regToken@SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
    checkSlidingWindowLimit (verifyHitsCountKey $ Id entityId)
    when verified $ throwError $ AuthBlocked "Already verified."
    checkForExpiry authExpiry updatedAt
    if authValueHash == req.otp
      then do
        person <- checkPersonExists entityId
        let isNewPerson = person.isNew
        clearOldRegToken person $ Id tokenId
        let deviceToken = (req.deviceToken) <|> (person.deviceToken)
        DB.runSqlDBTransaction $ do
          RegistrationToken.setVerified $ Id tokenId
          Person.updateDeviceToken person.id deviceToken
          when isNewPerson $
            Person.setIsNewFalse person.id
        when isNewPerson $
          Notify.notifyOnRegistration regToken person.id deviceToken
        decPerson <- decrypt person
        return $ AuthVerifyRes token (SP.makePersonAPIEntity $ decPerson{deviceToken})
      else throwError InvalidAuthData
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
        throwError TokenExpired

getRegistrationTokenE :: DBFlow m r => Text -> m SR.RegistrationToken
getRegistrationTokenE tokenId =
  RegistrationToken.findById tokenId >>= fromMaybeM (TokenNotFound tokenId)

createPerson :: (EncFlow m r, DBFlow m r) => AuthReq -> m SP.Person
createPerson req = do
  person <- makePerson req
  Person.create person
  pure person

checkPersonExists :: DBFlow m r => Text -> m SP.Person
checkPersonExists entityId =
  Person.findById (Id entityId) >>= fromMaybeM PersonDoesNotExist

resend :: Text -> FlowHandler ResendAuthRes
resend tokenId =
  withFlowHandlerAPI $ do
    SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
    person <- checkPersonExists entityId
    if attempts > 0
      then do
        smsCfg <- smsCfg <$> ask
        otpSmsTemplate <- otpSmsTemplate <$> ask
        mobileNumber <- decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
        countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
        withLogTag ("personId_" <> entityId) $
          SF.sendOTP smsCfg otpSmsTemplate (countryCode <> mobileNumber) authValueHash
            >>= SF.checkRegistrationSmsResult
        _ <- RegistrationToken.updateAttempts (attempts - 1) id
        return $ AuthRes tokenId (attempts - 1)
      else throwError $ AuthBlocked "Attempts limit exceed."

clearOldRegToken :: DBFlow m r => SP.Person -> Id SR.RegistrationToken -> m ()
clearOldRegToken person = RegistrationToken.deleteByPersonIdExceptNew (getId $ person.id)

logout :: Id SP.Person -> FlowHandler APISuccess
logout personId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  regTokens <- RegistrationToken.findAllByPersonId personId
  -- We should have only one RegToken at this point, but just in case we use findAll
  for_ regTokens $ \regToken -> do
    let key = authTokenCacheKey regToken.token
    void $ Redis.deleteKeyRedis key
  DB.runSqlDBTransaction $ do
    Person.updateDeviceToken personId Nothing
    RegistrationToken.deleteByPersonId personId
  pure Success
