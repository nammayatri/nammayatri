{-# LANGUAGE OverloadedLabels #-}

module Product.Registration where

import qualified Beckn.External.FCM.Flow as FCM
import qualified Beckn.External.FCM.Types as FCM
import qualified Beckn.External.MyValuesFirst.Flow as Sms
import qualified Beckn.External.MyValuesFirst.Types as Sms
import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import Beckn.Utils.Extra
import qualified Crypto.Number.Generate as Cryptonite
import qualified Data.Accessor as Lens
import Data.Aeson
import Data.Generics.Labels
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import System.Environment
import Types.API.Registration

initiateLogin :: InitiateLoginReq -> FlowHandler InitiateLoginRes
initiateLogin req =
  withFlowHandler $ do
    case (req ^. Lens.medium, req ^. Lens._type) of
      (SR.SMS, SR.OTP) -> initiateFlow req
      _ -> L.throwException $ err400 {errBody = "UNSUPPORTED_MEDIUM_TYPE"}

initiateFlow :: InitiateLoginReq -> L.Flow InitiateLoginRes
initiateFlow req = do
  let mobileNumber = req ^. Lens.identifier
  person <-
    QP.findByMobileNumber mobileNumber
      >>= maybe (createPerson req) pure
  let entityId = _getPersonId . SP._id $ person
  useFakeOtpM <- L.runIO $ lookupEnv "USE_FAKE_SMS"
  regToken <- makeSession req entityId SR.USER (T.pack <$> useFakeOtpM)
  QR.create regToken
  --  sendOTP mobileNumber (SR._authValueHash regToken)
  let attempts = SR._attempts regToken
      tokenId = SR._id regToken
      notificationData =
        FCM.FCMData
          { _fcmNotificationType = FCM.REGISTRATION_APPROVED,
            _fcmShowNotification = FCM.SHOW,
            _fcmEntityIds = show regToken,
            _fcmEntityType = FCM.Organization
          }
      title = FCM.FCMNotificationTitle $ T.pack "Registration Completed!"
      body = FCM.FCMNotificationBody $ T.pack "You can now start accepting rides!"
  FCM.notifyPerson title body notificationData person
  return $ InitiateLoginRes {attempts, tokenId}

makePerson :: InitiateLoginReq -> L.Flow SP.Person
makePerson req = do
  let role = fromMaybe SP.USER (req ^. Lens.role)
  id <- BC.generateGUID
  now <- getCurrentTimeUTC
  return $
    SP.Person
      { _id = id,
        _firstName = Nothing,
        _middleName = Nothing,
        _lastName = Nothing,
        _fullName = Nothing,
        _role = role,
        _gender = SP.UNKNOWN,
        _identifierType = SP.MOBILENUMBER,
        _email = Nothing,
        _mobileNumber = Just $ req ^. #_identifier,
        _mobileCountryCode = Nothing,
        _identifier = Just $ req ^. #_identifier,
        _rating = Nothing,
        _verified = False,
        _status = SP.INACTIVE,
        _udf1 = Nothing,
        _udf2 = Nothing,
        _deviceToken = req ^. #_deviceToken,
        _organizationId = Nothing,
        _locationId = Nothing,
        _description = Nothing,
        _createdAt = now,
        _updatedAt = now
      }

makeSession ::
  InitiateLoginReq -> Text -> SR.RTEntityType -> Maybe Text -> L.Flow SR.RegistrationToken
makeSession req entityId entityType fakeOtp = do
  otp <- case fakeOtp of
    Just otp -> return otp
    Nothing -> generateOTPCode
  id <- L.generateGUID
  token <- L.generateGUID
  now <- getCurrentTimeUTC
  attempts <-
    L.runIO $ fromMaybe 3 . (>>= readMaybe) <$> lookupEnv "SMS_ATTEMPTS"
  authExpiry <-
    L.runIO $ fromMaybe 3 . (>>= readMaybe) <$> lookupEnv "AUTH_EXPIRY"
  tokenExpiry <-
    L.runIO $ fromMaybe 365 . (>>= readMaybe) <$> lookupEnv "TOKEN_EXPIRY"
  return $
    SR.RegistrationToken
      { _id = id,
        _token = token,
        _attempts = attempts,
        _authMedium = (req ^. Lens.medium),
        _authType = (req ^. Lens._type),
        _authValueHash = otp,
        _verified = False,
        _authExpiry = authExpiry,
        _tokenExpiry = tokenExpiry,
        _EntityId = entityId,
        _entityType = entityType,
        _createdAt = now,
        _updatedAt = now,
        _info = Nothing
      }

generateOTPCode :: L.Flow Text
generateOTPCode =
  L.runIO $ padLeft 4 '0' . show <$> Cryptonite.generateBetween 1 9999
  where
    padLeft n c txt =
      let prefix = replicate (max 0 $ n - length txt) c
       in T.pack prefix <> txt

sendOTP :: Text -> Text -> L.Flow ()
sendOTP phoneNumber otpCode = do
  username <- L.runIO $ getEnv "SMS_GATEWAY_USERNAME"
  password <- L.runIO $ getEnv "SMS_GATEWAY_PASSWORD"
  res <-
    Sms.submitSms
      Sms.defaultBaseUrl
      Sms.SubmitSms
        { Sms._username = T.pack username,
          Sms._password = T.pack password,
          Sms._from = "JUSPAY",
          Sms._to = phoneNumber,
          Sms._text = "Your OTP is " <> otpCode
        }
  whenLeft res $ \err -> L.throwException err503 {errBody = encode err}

login :: Text -> LoginReq -> FlowHandler LoginRes
login tokenId req =
  withFlowHandler $ do
    SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId
    when _verified $ L.throwException $ err400 {errBody = "ALREADY_VERIFIED"}
    checkForExpiry _authExpiry _updatedAt
    let isValid =
          _authMedium == req ^. Lens.medium && _authType == req ^. Lens._type
            && _authValueHash
              == req
              ^. Lens.hash
    if isValid
      then do
        person <- checkPersonExists _EntityId
        QR.updateVerified tokenId True
        QP.update (SP._id person) SP.ACTIVE True (maybe (person ^. #_deviceToken) Just (req ^. #_deviceToken))
        updatedPerson <- QP.findPersonById (SP._id person)
        return $ LoginRes _token (Just $ maskPerson updatedPerson)
      else L.throwException $ err400 {errBody = "AUTH_VALUE_MISMATCH"}
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt)
        $ L.throwException
        $ err400 {errBody = "AUTH_EXPIRED"}

checkRegistrationTokenExists :: Text -> L.Flow SR.RegistrationToken
checkRegistrationTokenExists tokenId =
  QR.findRegistrationToken tokenId >>= fromMaybeM400 "INVALID_TOKEN"

createPerson :: InitiateLoginReq -> L.Flow SP.Person
createPerson req = do
  person <- makePerson req
  QP.create person
  pure person

checkPersonExists :: Text -> L.Flow SP.Person
checkPersonExists _EntityId =
  QP.findPersonById (PersonId _EntityId)

reInitiateLogin :: Text -> ReInitiateLoginReq -> FlowHandler InitiateLoginRes
reInitiateLogin tokenId req =
  withFlowHandler $ do
    SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId
    void $ checkPersonExists _EntityId
    if _attempts > 0
      then do
        sendOTP (req ^. Lens.identifier) _authValueHash
        QR.updateAttempts (_attempts - 1) _id
        return $ InitiateLoginRes tokenId (_attempts - 1)
      else L.throwException $ err400 {errBody = "LIMIT_EXCEEDED"}
