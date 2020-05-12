{-# LANGUAGE OverloadedLabels #-}

module Product.Registration where

import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common (withFlowHandler)
import qualified Crypto.Number.Generate as Cryptonite
import qualified Data.Accessor as Lens
import Data.Aeson
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified Epass.External.MyValuesFirst.Flow as Sms
import qualified Epass.External.MyValuesFirst.Types as Sms
import Epass.Utils.Common
import Epass.Utils.Extra
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import System.Environment
import Types.API.Registration
import Types.App

initiateLogin :: InitiateLoginReq -> FlowHandler InitiateLoginRes
initiateLogin req =
  withFlowHandler $ do
    L.logInfo "LOGIN" "here"
    case (req ^. Lens.medium, req ^. Lens._type) of
      (SR.SMS, SR.OTP) -> initiateFlow req
      _ -> L.throwException $ err400 {errBody = "UNSUPPORTED_MEDIUM_TYPE"}

initiateFlow :: InitiateLoginReq -> L.Flow InitiateLoginRes
initiateFlow req = do
  let mobileNumber = req ^. Lens.identifier
  entityId <-
    Person.findByRoleAndIdentifier SP.USER SP.MOBILENUMBER mobileNumber
      >>= maybe (createPerson req) (return . _getPersonId . SP._id)
  regToken <- makeSession req entityId
  RegistrationToken.create regToken
  sendOTP mobileNumber (SR._authValueHash regToken)
  let attempts = SR._attempts regToken
      tokenId = SR._id regToken
  return $ InitiateLoginRes {attempts, tokenId}

makePerson :: InitiateLoginReq -> L.Flow SP.Person
makePerson req = do
  role <- fromMaybeM400 "CUSTOMER_ROLE required" (req ^. Lens.role)
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
        _deviceToken = Nothing,
        _udf1 = Nothing,
        _udf2 = Nothing,
        _organizationId = Nothing,
        _locationId = Nothing,
        _description = Nothing,
        _createdAt = now,
        _updatedAt = now
      }

makeSession ::
  InitiateLoginReq -> Text -> L.Flow SR.RegistrationToken
makeSession req entityId = do
  otp <- generateOTPCode
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
        _entityType = SR.USER,
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
    SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
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
        let personId = person ^. #_id
        Person.update personId SP.ACTIVE Nothing Nothing Nothing
        Person.findById personId
          >>= fromMaybeM500 "Could not find user"
          >>= return . LoginRes _token
      else L.throwException $ err400 {errBody = "AUTH_VALUE_MISMATCH"}
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt)
        $ L.throwException
        $ err400 {errBody = "AUTH_EXPIRED"}

getRegistrationTokenE :: Text -> L.Flow SR.RegistrationToken
getRegistrationTokenE tokenId =
  RegistrationToken.findById tokenId >>= fromMaybeM400 "INVALID_TOKEN"

createPerson :: InitiateLoginReq -> L.Flow Text
createPerson req = do
  person <- makePerson req
  Person.create person
  return $ _getPersonId $ SP._id person

checkPersonExists :: Text -> L.Flow SP.Person
checkPersonExists _EntityId =
  Person.findById (PersonId _EntityId) >>= fromMaybeM400 "INVALID_DATA"

reInitiateLogin :: Text -> ReInitiateLoginReq -> FlowHandler InitiateLoginRes
reInitiateLogin tokenId req =
  withFlowHandler $ do
    SR.RegistrationToken {..} <- getRegistrationTokenE tokenId
    void $ checkPersonExists _EntityId
    if _attempts > 0
      then do
        sendOTP (req ^. Lens.identifier) _authValueHash
        RegistrationToken.updateAttempts (_attempts - 1) _id
        return $ InitiateLoginRes tokenId (_attempts - 1)
      else L.throwException $ err400 {errBody = "LIMIT_EXCEEDED"}
