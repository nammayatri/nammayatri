module Beckn.Product.Registration where

import qualified Beckn.Data.Accessor                     as Lens
import qualified Beckn.External.MyValuesFirst.Flow       as Sms
import qualified Beckn.External.MyValuesFirst.Types      as Sms
import qualified Beckn.Storage.Queries.Customer          as QC
import qualified Beckn.Storage.Queries.RegistrationToken as QR
import           Beckn.Types.API.Registration
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.Customer            as SC
import qualified Beckn.Types.Storage.RegistrationToken   as SR
import           Beckn.Utils.Common
import           Beckn.Utils.Extra
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import qualified Crypto.Number.Generate                  as Cryptonite
import           Data.Aeson
import qualified Data.Text                               as T
import qualified EulerHS.Language                        as L
import           EulerHS.Prelude
import           Servant
import           System.Environment

initiateLogin :: InitiateLoginReq -> FlowHandler InitiateLoginRes
initiateLogin req =
  withFlowHandler $ do
    case (req ^. Lens.medium, req ^. Lens._type) of
      (SR.SMS, SR.OTP) -> initiateSms req
      _ -> L.throwException $ err400 {errBody = "UNSUPPORTED_MEDIUM_TYPE"}

initiateSms :: InitiateLoginReq -> L.Flow InitiateLoginRes
initiateSms req = do
  otp <- generateOTPCode
  submitOtpCode (req ^. Lens._value) otp
  uuid <- L.generateGUID
  uuidRt <- L.generateGUID
  uuidR <- L.generateGUID
  now <- getCurrentTimeUTC
  let cust = makeCustomer uuid now
  QC.create cust
  attempts <-
    L.runIO $ fromMaybe 3 . (>>= readMaybe) <$> lookupEnv "SMS_ATTEMPTS"
  authExpiry <-
    L.runIO $ fromMaybe 3 . (>>= readMaybe) <$> lookupEnv "AUTH_EXPIRY"
  tokenExpiry <-
    L.runIO $ fromMaybe 365 . (>>= readMaybe) <$> lookupEnv "TOKEN_EXPIRY"
  let regToken = makeSession uuidR uuidRt uuid now otp attempts authExpiry tokenExpiry
  QR.create regToken
  return $ InitiateLoginRes {attempts = attempts, tokenId = uuidR}
  where
    makeCustomer uuid now =
      SC.Customer
        (CustomerId uuid)
        Nothing
        Nothing
        Nothing
        False
        (req ^. Lens.role)
        Nothing
        now
        now

    makeSession uuidR uuidRt uuid now otp a ae te =
      SR.RegistrationToken
        uuidR
        uuidRt
        a
        (req ^. Lens.medium)
        (req ^. Lens._type)
        otp
        False
        ae
        te
        uuid
        SR.CUSTOMER
        now
        now
        Nothing

    generateOTPCode :: L.Flow Text
    generateOTPCode =
      L.runIO $ padLeft 4 '0' . show <$> Cryptonite.generateBetween 1 9999

    padLeft n c txt =
      let prefix = replicate (max 0 $ n - length txt) c
       in T.pack prefix <> txt

submitOtpCode :: Text -> Text -> L.Flow ()
submitOtpCode phoneNumber otpCode = do
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
    SR.RegistrationToken {..} <-
      QR.findRegistrationToken tokenId >>= fromMaybeM400 "INVALID_TOKEN"
    cust <-
      QC.findCustomerById (CustomerId _EntityId) >>=
      fromMaybeM400 "INVALID_DATA"
    unlessM (isExpired (realToFrac (_authExpiry * 60)) _updatedAt) $
      L.throwException $ err400 { errBody = "AUTH_EXPIRED" }
    when _verified $ L.throwException $ err400 {errBody = "ALREADY_VERIFIED"}
    let verify =
          _authMedium == req ^. Lens.medium && _authType == req ^. Lens._type &&
          _authValueHash == req ^. Lens.hash
    if verify
      then do
        QC.updateStatus True (SC._id cust)
        return $ LoginRes _token cust
      else L.throwException $ err400 {errBody = "VALUE_MISMATCH"}

reInitiateLogin :: Text -> ReInitiateLoginReq -> FlowHandler InitiateLoginRes
reInitiateLogin tokenId req =
  withFlowHandler $ do
    SR.RegistrationToken {..} <-
      QR.findRegistrationToken tokenId >>= fromMaybeM400 "INVALID_TOKEN"
    cust <-
      QC.findCustomerById (CustomerId _EntityId) >>=
      fromMaybeM400 "INVALID_DATA"
    if _attempts > 0
      then do
        submitOtpCode (req ^. Lens._value) _authValueHash
        QR.updateAttempts (_attempts - 1) _id
        return $ InitiateLoginRes tokenId (_attempts - 1)
      else L.throwException $ err400 {errBody = "LIMIT_EXCEEDED"}
