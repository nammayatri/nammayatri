module Product.Registration (checkPersonExists, initiateLogin, login, reInitiateLogin, logout) where

import App.Types
import Beckn.External.Encryption
import qualified Beckn.External.MyValueFirst.Flow as SF
import Beckn.Sms.Config
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.APISuccess
import Beckn.Types.Common as BC
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.SlidingWindowLimiter
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Product.Person as Person
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Types.API.Registration
import Types.Error
import Utils.Common
import qualified Utils.Notifications as Notify

initiateLogin :: InitiateLoginReq -> FlowHandler InitiateLoginRes
initiateLogin req =
  withFlowHandlerAPI $
    case (req.medium, req.__type) of
      (SR.SMS, SR.OTP) -> ask >>= initiateFlow req . smsCfg
      _ -> throwError $ InvalidRequest "medium and type fields must be SMS and OTP"

initiateFlowHitsCountKey :: SP.Person -> Text
initiateFlowHitsCountKey person = "Registration:initiateFlow" <> getId person.id <> ":hitsCount"

initiateFlow :: InitiateLoginReq -> SmsConfig -> Flow InitiateLoginRes
initiateFlow req smsCfg = do
  let mobileNumber = req.mobileNumber
      countryCode = req.mobileCountryCode
  mbPerson <- QP.findByMobileNumber countryCode mobileNumber
  person <- case mbPerson of
    Nothing -> do
      whenJust
        req.role
        (\role -> when (role == SP.DRIVER) $ throwError $ InvalidRequest "Driver must be registered by Transport Admin")
      createPerson req
    Just p -> pure p
  checkSlidingWindowLimit (initiateFlowHitsCountKey person)
  let entityId = getId . SP.id $ person
      useFakeOtpM = useFakeSms smsCfg
      scfg = sessionConfig smsCfg
  regToken <- case useFakeOtpM of
    Just _ -> do
      token <- makeSession scfg req entityId SR.USER (show <$> useFakeOtpM)
      QR.create token
      return token
    Nothing -> do
      token <- makeSession scfg req entityId SR.USER Nothing
      QR.create token
      otpSmsTemplate <- otpSmsTemplate <$> ask
      withLogTag ("personId_" <> getId person.id) $
        SF.sendOTP smsCfg otpSmsTemplate (countryCode <> mobileNumber) (SR.authValueHash token)
      return token
  let attempts = SR.attempts regToken
      tokenId = SR.id regToken
  Notify.notifyOnRegistration regToken person
  return $ InitiateLoginRes {attempts, tokenId}

makePerson :: InitiateLoginReq -> Flow SP.Person
makePerson req = do
  let role = fromMaybe SP.USER (req.role)
  pid <- BC.generateGUID
  now <- getCurrentTime
  return $
    SP.Person
      { id = pid,
        firstName = Nothing,
        middleName = Nothing,
        lastName = Nothing,
        fullName = Nothing,
        role = role,
        gender = SP.UNKNOWN,
        identifierType = SP.MOBILENUMBER,
        email = Nothing,
        passwordHash = Nothing,
        mobileNumber = Just $ req.mobileNumber,
        mobileCountryCode = Just $ req.mobileCountryCode,
        identifier = Nothing,
        rating = Nothing,
        verified = False,
        status = SP.INACTIVE,
        udf1 = Nothing,
        udf2 = Nothing,
        deviceToken = req.deviceToken,
        organizationId = Nothing,
        locationId = Nothing,
        description = Nothing,
        createdAt = now,
        updatedAt = now
      }

makeSession ::
  SmsSessionConfig -> InitiateLoginReq -> Text -> SR.RTEntityType -> Maybe Text -> Flow SR.RegistrationToken
makeSession SmsSessionConfig {..} req entityId entityType fakeOtp = do
  otp <- maybe generateOTPCode return fakeOtp
  rtid <- L.generateGUID
  token <- L.generateGUID
  now <- getCurrentTime
  return $
    SR.RegistrationToken
      { id = rtid,
        token = token,
        attempts = attempts,
        authMedium = req.medium,
        authType = req.__type,
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

loginHitsCountKey :: SP.Person -> Text
loginHitsCountKey person = "Registration:login:" <> getId person.id <> ":hitsCount"

login :: Text -> LoginReq -> FlowHandler LoginRes
login tokenId req =
  withFlowHandlerAPI $ do
    SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId
    when verified $ throwError $ AuthBlocked "Already verified."
    checkForExpiry authExpiry updatedAt
    let isValid =
          authMedium == req.medium && authType == req.__type
            && authValueHash
              == req.hash
    if isValid
      then do
        person <- checkPersonExists entityId
        checkSlidingWindowLimit (loginHitsCountKey person)
        clearOldRegToken person $ Id tokenId
        QR.updateVerified tokenId True
        let deviceToken = (req.deviceToken) <|> (person.deviceToken)
        QP.update (SP.id person) SP.ACTIVE True deviceToken
        updatedPerson <- QP.findPersonById (SP.id person)
        return $ LoginRes token (Just $ SP.maskPerson updatedPerson)
      else throwError InvalidAuthData
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
        throwError TokenExpired

checkRegistrationTokenExists :: Text -> Flow SR.RegistrationToken
checkRegistrationTokenExists tokenId =
  QR.findRegistrationToken tokenId >>= fromMaybeM (TokenNotFound tokenId)

createPerson :: InitiateLoginReq -> Flow SP.Person
createPerson req = do
  person <- makePerson req
  QP.create person
  when (person.role == SP.DRIVER) $ Person.createDriverDetails (person.id)
  pure person

checkPersonExists :: Text -> Flow SP.Person
checkPersonExists entityId =
  QP.findPersonById (Id entityId)

reInitiateLogin :: Text -> ReInitiateLoginReq -> FlowHandler InitiateLoginRes
reInitiateLogin tokenId req =
  withFlowHandlerAPI $ do
    SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId
    void $ checkPersonExists entityId
    if attempts > 0
      then do
        smsCfg <- smsCfg <$> ask
        otpSmsTemplate <- otpSmsTemplate <$> ask
        let mobileNumber = req.mobileNumber
            countryCode = req.mobileCountryCode
        withLogTag ("personId_" <> entityId) $
          SF.sendOTP smsCfg otpSmsTemplate (countryCode <> mobileNumber) authValueHash
        _ <- QR.updateAttempts (attempts - 1) id
        return $ InitiateLoginRes tokenId (attempts - 1)
      else throwError $ AuthBlocked "Limit exceeded."

clearOldRegToken :: SP.Person -> Id SR.RegistrationToken -> Flow ()
clearOldRegToken person = QR.deleteByEntitiyIdExceptNew (getId $ person.id)

logout :: SR.RegistrationToken -> FlowHandler APISuccess
logout SR.RegistrationToken {..} = withFlowHandlerAPI $ do
  uperson <- QP.findPersonById (Id entityId)
  eperson <- encrypt uperson
  DB.runSqlDBTransaction $ do
    QP.updatePersonRec (uperson.id) eperson {SP.deviceToken = Nothing}
    QR.deleteByEntitiyId entityId
  pure Success
