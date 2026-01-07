module Domain.Action.UI.SocialLogin where

import qualified API.Types.UI.SocialLogin
import qualified API.Types.UI.SocialLogin as SL
import Data.Aeson
import qualified Data.Text as T
import qualified Domain.Action.UI.Registration as DR
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
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
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
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
  merchant <- case req.merchantShortId of
    Just merchantShortId -> CQMOC.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
    Nothing -> throwError $ InvalidRequest "Cannot perform social auth without MerchantShortId"
  result <- L.runIO $ fetchTokenInfo iosValidateEnpoint req.oauthProvider req.tokenId
  case result of
    Right info -> do
      oldPerson <- PQ.findByEmailAndMerchantIdAndRole (Just $ info.email) merchant.id SP.DRIVER
      moc <- CQMOC.findByMerchantIdAndCity merchant.id req.merchantOperatingCity >>= fromMaybeM (MerchantOperatingCityNotFound $ show req.merchantOperatingCity)
      (person, isNew) <-
        case oldPerson of
          Just person' -> pure (person', False)
          Nothing ->
            (,True) <$> do
              deploymentVersion <- asks (.version)
              let createPersonInput = buildCreatePersonInput moc.city req.name info.email merchant
              DR.createDriverWithDetails createPersonInput Nothing Nothing Nothing Nothing Nothing (Just deploymentVersion.getDeploymentVersion) merchant.id moc.id False
      QR.deleteByPersonId (getId person.id)
      token <- makeSession person.id.getId merchant.id.getId moc.id.getId
      _ <- QR.create token
      pure $ SL.SocialLoginRes isNew token.token
    Left _ -> throwError . FailedToVerifyIdToken $ show req.oauthProvider <> ", idToken: " <> req.tokenId <> " error: "
  where
    buildCreatePersonInput city name email merchant =
      DR.AuthReq
        { mobileNumber = Nothing,
          mobileCountryCode = Nothing,
          name = name,
          merchantId = merchant.id.getId,
          merchantOperatingCity = Just city,
          email = Just email,
          identifierType = Just SP.EMAIL,
          registrationLat = req.registrationLat,
          registrationLon = req.registrationLon,
          otpChannel = Nothing
        }

makeSession ::
  Text ->
  Text ->
  Text ->
  Environment.Flow SR.RegistrationToken
makeSession entityId merchantId merchantOpCityId = do
  otp <- generateOTPCode
  rtid <- generateGUID
  token <- generateGUID
  now <- getCurrentTime
  return $
    SR.RegistrationToken
      { id = Id rtid,
        token = token,
        attempts = 3, -- TODO: maybe change later
        authMedium = SR.EMAIL,
        authType = SR.OAUTH,
        authValueHash = otp,
        verified = True,
        authExpiry = 3,
        tokenExpiry = 365,
        entityId = entityId,
        merchantId = merchantId,
        merchantOperatingCityId = merchantOpCityId,
        entityType = SR.USER,
        createdAt = now,
        updatedAt = now,
        info = Nothing,
        alternateNumberAttempts = 3 -- TODO: change later
      }

postSocialUpdateProfile ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.SocialLogin.SocialUpdateProfileReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postSocialUpdateProfile (mbPersonId, merchantId, _) req = do
  personId <- maybe (throwError $ InternalError "Not Implemented for dashboard") pure mbPersonId
  encNewPhoneNumber <- mapM encrypt req.mobileNumber
  person <- PQ.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  case req.mobileNumber of
    Just mobileNumber -> do
      mobileNumberHash <- getDbHash mobileNumber
      let countryCode = fromMaybe "+91" req.mobileCountryCode
      PQ.findByMobileNumberAndMerchantAndRole countryCode mobileNumberHash merchantId SP.DRIVER >>= \case
        Just existingPerson
          | personId /= existingPerson.id ->
            throwError $ DriverMobileAlreadyExists (show req.mobileNumber)
        _ -> return ()
    _ -> return ()
  PQ.findByEmailAndMerchantIdAndRole (Just req.email) merchantId SP.DRIVER >>= \case
    Just existingPerson
      | personId /= existingPerson.id ->
        throwError $ DriverEmailAlreadyExists (show req.email)
    _ -> return ()
  let updatedPerson =
        person
          { SP.mobileCountryCode = req.mobileCountryCode <|> person.mobileCountryCode,
            SP.mobileNumber = encNewPhoneNumber <|> person.mobileNumber,
            SP.firstName = fromMaybe person.firstName req.firstName,
            SP.lastName = req.lastName <|> person.lastName,
            SP.email = Just req.email
          }
  PQ.updatePersonDetails updatedPerson
  pure Kernel.Types.APISuccess.Success
