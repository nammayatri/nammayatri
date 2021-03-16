{-# LANGUAGE OverloadedLabels #-}

module Product.Registration (initiateLogin, login, reInitiateLogin) where

import App.Types
import qualified Beckn.External.MyValueFirst.Flow as SF
import qualified Beckn.External.MyValueFirst.Types as SMS
import Beckn.Sms.Config
import qualified Beckn.Types.Common as BC
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import qualified Crypto.Number.Generate as Cryptonite
import qualified Data.Aeson as Aeson
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Types.API.Registration
import qualified Utils.Notifications as Notify

initiateLogin :: InitiateLoginReq -> FlowHandler InitiateLoginRes
initiateLogin req =
  withFlowHandler $
    case (req ^. #_medium, req ^. #__type) of
      (SR.SMS, SR.OTP) -> ask >>= initiateFlow req . smsCfg
      _ -> throwError400 "UNSUPPORTED_MEDIUM_TYPE"

initiateFlow :: InitiateLoginReq -> SmsConfig -> Flow InitiateLoginRes
initiateFlow req smsCfg = do
  let mobileNumber = req ^. #_mobileNumber
      countryCode = req ^. #_mobileCountryCode
  person <-
    Person.findByRoleAndMobileNumber SP.USER countryCode mobileNumber
      >>= maybe (createPerson req) clearOldRegToken
  let entityId = getId . SP._id $ person
      useFakeOtpM = useFakeSms smsCfg
      scfg = sessionConfig smsCfg
  regToken <- case useFakeOtpM of
    Just _ -> do
      token <- makeSession scfg req entityId (show <$> useFakeOtpM)
      RegistrationToken.create token
      return token
    Nothing -> do
      token <- makeSession scfg req entityId Nothing
      RegistrationToken.create token
      sendOTP (credConfig smsCfg) (countryCode <> mobileNumber) (SR._authValueHash token)
      return token
  let attempts = SR._attempts regToken
      tokenId = SR._id regToken
  Notify.notifyOnRegistration regToken person
  return $ InitiateLoginRes {attempts, tokenId}

makePerson :: InitiateLoginReq -> Flow SP.Person
makePerson req = do
  role <- fromMaybeM400 "CUSTOMER_ROLE required" (req ^. #_role)
  pid <- BC.generateGUID
  now <- getCurrTime
  return $
    SP.Person
      { _id = pid,
        _firstName = Nothing,
        _middleName = Nothing,
        _lastName = Nothing,
        _fullName = Nothing,
        _role = role,
        _gender = SP.UNKNOWN,
        _identifierType = SP.MOBILENUMBER,
        _email = Nothing,
        _passwordHash = Nothing,
        _mobileNumber = Just $ req ^. #_mobileNumber,
        _mobileCountryCode = Just $ req ^. #_mobileCountryCode,
        _identifier = Nothing,
        _rating = Nothing,
        _verified = False,
        _status = SP.INACTIVE,
        _deviceToken = req ^. #_deviceToken,
        _udf1 = Nothing,
        _udf2 = Nothing,
        _organizationId = Nothing,
        _locationId = Nothing,
        _description = Nothing,
        _createdAt = now,
        _updatedAt = now
      }

makeSession ::
  SmsSessionConfig -> InitiateLoginReq -> Text -> Maybe Text -> Flow SR.RegistrationToken
makeSession SmsSessionConfig {..} req entityId fakeOtp = do
  otp <- maybe generateOTPCode return fakeOtp
  rtid <- L.generateGUID
  token <- L.generateGUID
  now <- getCurrTime
  return $
    SR.RegistrationToken
      { _id = rtid,
        _token = token,
        _attempts = attempts,
        _authMedium = req ^. #_medium,
        _authType = req ^. #__type,
        _authValueHash = otp,
        _verified = False,
        _authExpiry = authExpiry,
        _tokenExpiry = tokenExpiry,
        _EntityId = entityId,
        _entityType = SR.USER,
        _createdAt = now,
        _updatedAt = now,
        _info = Nothing
      }

generateOTPCode :: Flow Text
generateOTPCode =
  L.runIO $ padNumber 4 <$> Cryptonite.generateBetween 1 9999

sendOTP :: SmsCredConfig -> Text -> Text -> Flow ()
sendOTP SmsCredConfig {..} phoneNumber otpCode = do
  res <-
    SF.submitSms
      SF.defaultBaseUrl
      SMS.SubmitSms
        { SMS._username = username,
          SMS._password = password,
          SMS._from = SMS.JUSPAY,
          SMS._to = phoneNumber,
          SMS._category = SMS.BULK,
          SMS._text = SF.constructOtpSms otpCode otpHash
        }
  whenLeft res $ \err -> throwError503 $ Aeson.encode err

login :: Text -> LoginReq -> FlowHandler LoginRes
login tokenId req =
  withFlowHandler $ do
    SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
    when _verified $ throwError400 "ALREADY_VERIFIED"
    checkForExpiry _authExpiry _updatedAt
    let isValid =
          _authMedium == req ^. #_medium
            && _authType == req ^. #__type
            && _authValueHash == req ^. #_hash
    if isValid
      then do
        person <- checkPersonExists _EntityId
        let personId = person ^. #_id
            updatedPerson =
              person
                { SP._status = SP.ACTIVE,
                  SP._deviceToken =
                    (req ^. #_deviceToken) <|> (person ^. #_deviceToken)
                }
        Person.updateMultiple personId updatedPerson
        LoginRes _token . SP.maskPerson
          <$> ( Person.findById personId
                  >>= fromMaybeM500 "Could not find user"
              )
      else throwError400 "AUTH_VALUE_MISMATCH"
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
        throwError400 "AUTH_EXPIRED"

getRegistrationTokenE :: Text -> Flow SR.RegistrationToken
getRegistrationTokenE tokenId =
  RegistrationToken.findById tokenId >>= fromMaybeM401 "INVALID_TOKEN"

createPerson :: InitiateLoginReq -> Flow SP.Person
createPerson req = do
  person <- makePerson req
  Person.create person
  pure person

checkPersonExists :: Text -> Flow SP.Person
checkPersonExists _EntityId =
  Person.findById (Id _EntityId) >>= fromMaybeM400 "INVALID_DATA"

reInitiateLogin :: Text -> ReInitiateLoginReq -> FlowHandler InitiateLoginRes
reInitiateLogin tokenId req =
  withFlowHandler $ do
    SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
    void $ checkPersonExists _EntityId
    if _attempts > 0
      then do
        credCfg <- credConfig . smsCfg <$> ask
        let mobileNumber = req ^. #_mobileNumber
            countryCode = req ^. #_mobileCountryCode
        sendOTP credCfg (countryCode <> mobileNumber) _authValueHash
        _ <- RegistrationToken.updateAttempts (_attempts - 1) _id
        return $ InitiateLoginRes tokenId (_attempts - 1)
      else throwError400 "LIMIT_EXCEEDED"

clearOldRegToken :: SP.Person -> Flow SP.Person
clearOldRegToken person = do
  RegistrationToken.deleteByPersonId $ getId $ person ^. #_id
  pure person
