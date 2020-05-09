module Epass.Product.Registration where

import qualified Epass.Data.Accessor                     as Lens
import qualified Epass.External.MyValuesFirst.Flow       as Sms
import qualified Epass.External.MyValuesFirst.Types      as Sms
import qualified Epass.Storage.Queries.Customer          as QC
import qualified Epass.Storage.Queries.CustomerDetail    as QCD
import qualified Epass.Storage.Queries.RegistrationToken as QR
import qualified Epass.Storage.Queries.User              as User
import           Epass.Types.API.Registration
import           Epass.Types.App
import           Epass.Types.Common
import qualified Epass.Types.Storage.Customer            as SC
import qualified Epass.Types.Storage.CustomerDetail      as SCD
import qualified Epass.Types.Storage.RegistrationToken   as SR
import qualified Epass.Types.Storage.User                as SU
import           Epass.Utils.Common
import           Epass.Utils.Extra
import           Epass.Utils.Routes
import           Epass.Utils.Storage
import qualified Crypto.Number.Generate                  as Cryptonite
import           Data.Aeson
import qualified Data.Text                               as T
import           Data.Time.LocalTime
import qualified EulerHS.Language                        as L
import           EulerHS.Prelude
import           Servant
import           System.Environment

initiateLogin :: InitiateLoginReq -> FlowHandler InitiateLoginRes
initiateLogin req =
  withFlowHandler $ do
    case (req ^. Lens.medium, req ^. Lens._type) of
      (SR.SMS, SR.OTP) -> initiateFlow req
      _ -> L.throwException $ err400 {errBody = "UNSUPPORTED_MEDIUM_TYPE"}

initiateFlow :: InitiateLoginReq -> L.Flow InitiateLoginRes
initiateFlow req = do
  let entityType = req ^. Lens.entityType
  let mobileNumber = req ^. Lens.identifier
  entityId <-
    case entityType of
      SR.CUSTOMER -> do
        QCD.findByIdentifier SCD.MOBILENUMBER mobileNumber >>=
          maybe (createCustomer req) (return . _getCustomerId . SCD._CustomerId)
      SR.USER -> do
        user <-
          fromMaybeM400 "User not found" =<<
          User.findByMobileNumber mobileNumber
        return $ _getUserId $ SU._id user
  regToken <- makeSession req entityId entityType
  QR.create regToken
  sendOTP mobileNumber (SR._authValueHash regToken)
  let attempts = SR._attempts regToken
      tokenId = SR._id regToken
  return $ InitiateLoginRes {attempts, tokenId}

makeCustomer :: InitiateLoginReq -> L.Flow SC.Customer
makeCustomer req = do
  role <- fromMaybeM400 "CUSTOMER_ROLE required" (req ^. Lens.role)
  id <- generateGUID
  now <- getCurrentTimeUTC
  return $
    SC.Customer
      { _id = id
      , _name = Nothing
      , _OrganizationId = Nothing
      , _TenantOrganizationId = Nothing
      , _verified = False
      , _role = role
      , _info = Nothing
      , _createdAt = now
      , _updatedAt = now
      }

makeSession ::
     InitiateLoginReq -> Text -> SR.RTEntityType -> L.Flow SR.RegistrationToken
makeSession req entityId entityType = do
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
      { _id = id
      , _token = token
      , _attempts = attempts
      , _authMedium = (req ^. Lens.medium)
      , _authType = (req ^. Lens._type)
      , _authValueHash = otp
      , _verified = False
      , _authExpiry = authExpiry
      , _tokenExpiry = tokenExpiry
      , _EntityId = entityId
      , _entityType = entityType
      , _createdAt = now
      , _updatedAt = now
      , _info = Nothing
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
        { Sms._username = T.pack username
        , Sms._password = T.pack password
        , Sms._from = "JUSPAY"
        , Sms._to = phoneNumber
        , Sms._text = "Your OTP is " <> otpCode
        }
  whenLeft res $ \err -> L.throwException err503 {errBody = encode err}

login :: Text -> LoginReq -> FlowHandler LoginRes
login tokenId req =
  withFlowHandler $ do
    SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId
    when _verified $ L.throwException $ err400 {errBody = "ALREADY_VERIFIED"}
    checkForExpiry _authExpiry _updatedAt
    let isValid =
          _authMedium == req ^. Lens.medium && _authType == req ^. Lens._type &&
          _authValueHash ==
          req ^.
          Lens.hash
    if isValid
      then do
        case _entityType of
          SR.CUSTOMER -> do
            cust <- checkCustomerExists _EntityId
            QC.updateStatus True (SC._id cust)
            custD <-
              makeCustomerDetails
                (CustomerId _EntityId)
                (req ^. Lens.identifier)
                SCD.MOBILENUMBER
            QCD.createIfNotExistsCustomerD custD
            return $ LoginRes _token (Just cust) Nothing
          SR.USER -> do
            user <- checkUserExists _EntityId
            User.update (SU._id user) SU.ACTIVE Nothing Nothing
            return $ LoginRes _token Nothing (Just user)
      else L.throwException $ err400 {errBody = "AUTH_VALUE_MISMATCH"}
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
      L.throwException $
      err400 {errBody = "AUTH_EXPIRED"}

makeCustomerDetails ::
     CustomerId -> Text -> SCD.IdentifierType -> L.Flow SCD.CustomerDetail
makeCustomerDetails custId mobileNumber mtype = do
  uuid <- generateGUID
  now <- getCurrentTimeUTC
  return $
    SCD.CustomerDetail
      { _id = uuid
      , _CustomerId = custId
      , _uniqueIdentifier = mobileNumber
      , _identifierType = mtype
      , _value = Null
      , _verified = True
      , _primaryIdentifier = True
      , _info = ""
      , _createdAt = now
      , _updatedAt = now
      }

checkRegistrationTokenExists :: Text -> L.Flow SR.RegistrationToken
checkRegistrationTokenExists tokenId =
  QR.findRegistrationToken tokenId >>= fromMaybeM400 "INVALID_TOKEN"

createCustomer :: InitiateLoginReq -> L.Flow Text
createCustomer req = do
  cust <- makeCustomer req
  QC.create cust
  return $ _getCustomerId $ SC._id cust

checkCustomerExists :: Text -> L.Flow SC.Customer
checkCustomerExists _EntityId =
  QC.findCustomerById (CustomerId _EntityId) >>= fromMaybeM400 "INVALID_DATA"

checkUserExists :: Text -> L.Flow SU.User
checkUserExists _EntityId =
  User.findById (UserId _EntityId)

reInitiateLogin :: Text -> ReInitiateLoginReq -> FlowHandler InitiateLoginRes
reInitiateLogin tokenId req =
  withFlowHandler $ do
    SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId
    case _entityType of
      SR.CUSTOMER -> void $ checkCustomerExists _EntityId
      SR.USER     -> void $ checkUserExists _EntityId
    if _attempts > 0
      then do
        sendOTP (req ^. Lens.identifier) _authValueHash
        QR.updateAttempts (_attempts - 1) _id
        return $ InitiateLoginRes tokenId (_attempts - 1)
      else L.throwException $ err400 {errBody = "LIMIT_EXCEEDED"}
