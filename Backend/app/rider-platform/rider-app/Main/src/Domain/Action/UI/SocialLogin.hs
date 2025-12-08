module Domain.Action.UI.SocialLogin where

import qualified API.Types.UI.SocialLogin as SL
import Data.Aeson
import qualified Data.Text as T
import qualified Domain.Action.UI.Registration as PR
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (encrypt, getDbHash)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import qualified Storage.CachedQueries.Merchant as CQMOC
import qualified Storage.Queries.Person as PQ
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
  merchant <- CQMOC.findByShortId req.merchantShortId >>= fromMaybeM (MerchantDoesNotExist req.merchantShortId.getShortId)
  result <- L.runIO $ fetchTokenInfo iosValidateEnpoint req.oauthProvider req.tokenId
  case result of
    Right info -> do
      oldPerson <- PQ.findByEmailAndMerchantId merchant.id info.email
      (person, isNew) <-
        case oldPerson of
          Just person' -> pure (person', False)
          Nothing ->
            (,True) <$> do
              let authReq = buildAuthReq info.email
              cloudType <- asks (.cloudType)
              PR.createPerson authReq SP.EMAIL Nothing Nothing Nothing Nothing Nothing Nothing cloudType merchant Nothing Nothing
      QR.deleteByPersonId person.id
      token <- makeSession person.id.getId merchant.id.getId
      _ <- QR.create token
      pure $ SL.SocialLoginRes isNew token.token
    Left _ -> throwError . InternalError $ show req.oauthProvider <> ", idToken: " <> req.tokenId <> " error: "
  where
    buildAuthReq email =
      PR.AuthReq
        { mobileNumber = Nothing,
          mobileCountryCode = Nothing,
          identifierType = Just SP.EMAIL,
          merchantId = req.merchantShortId,
          deviceToken = Nothing,
          notificationToken = Nothing,
          imeiNumber = Nothing,
          whatsappNotificationEnroll = Nothing,
          firstName = req.name,
          middleName = Nothing,
          lastName = Nothing,
          email = Just email,
          language = Nothing,
          businessEmail = Nothing,
          gender = Nothing,
          otpChannel = Nothing,
          registrationLat = req.registrationLat,
          registrationLon = req.registrationLon,
          enableOtpLessRide = req.enableOtpLessRide,
          allowBlockedUserLogin = Nothing,
          isOperatorReq = Nothing,
          reuseToken = Nothing
        }

makeSession ::
  Text ->
  Text ->
  Environment.Flow SR.RegistrationToken
makeSession entityId merchantId = do
  otp <- generateOTPCode
  rtid <- L.generateGUID
  token <- L.generateGUID
  now <- getCurrentTime
  return $
    SR.RegistrationToken
      { id = Id rtid,
        token = token,
        attempts = 3,
        authMedium = SR.EMAIL,
        authType = SR.OAUTH,
        authValueHash = otp,
        verified = True,
        authExpiry = 3,
        tokenExpiry = 356,
        entityId = entityId,
        merchantId = merchantId,
        entityType = SR.USER,
        createdAt = now,
        updatedAt = now,
        info = Nothing,
        createdViaPartnerOrgId = Nothing
      }

postSocialUpdateProfile ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    SL.SocialUpdateProfileReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postSocialUpdateProfile (mbPersonId, merchantId) req = do
  personId <- maybe (throwError $ InternalError "Not Implemented for dashboard") pure mbPersonId
  encNewPhoneNumber <- mapM encrypt req.mobileNumber
  person <- PQ.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  case req.mobileNumber of
    Just mobileNumber -> do
      mobileNumberHash <- getDbHash mobileNumber
      let countryCode = fromMaybe "+91" req.mobileCountryCode
      PQ.findByMobileNumberAndMerchantId countryCode mobileNumberHash merchantId >>= \case
        Just existingPerson
          | personId /= existingPerson.id ->
            throwError $ PersonMobileAlreadyExists (show req.mobileNumber)
        _ -> return ()
    _ -> return ()
  let updatedPerson =
        person
          { SP.mobileCountryCode = req.mobileCountryCode <|> person.mobileCountryCode,
            SP.mobileNumber = encNewPhoneNumber,
            SP.firstName = req.firstName <|> person.firstName,
            SP.lastName = req.lastName <|> person.lastName
          }
  PQ.updateByPrimaryKey updatedPerson
  pure Kernel.Types.APISuccess.Success
