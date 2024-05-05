{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.SocialLogin where

import qualified API.Types.UI.SocialLogin
import qualified API.Types.UI.SocialLogin as SL
import Control.Monad (mzero)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Domain.Action.UI.Registration as DR
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Person as PQ
import qualified Storage.Queries.RegistrationToken as QR
import Tools.Auth
import Tools.Error

googleTokenInfoUrl :: Text -> String -- TODO: change this to local validation as mentioned in this doc: https://developers.google.com/identity/sign-in/web/backend-auth#verify-the-integrity-of-the-id-token
googleTokenInfoUrl token = "https://oauth2.googleapis.com/tokeninfo?id_token=" <> T.unpack token

data GoogleTokenInfo = GoogleTokenInfo
  { email :: Text,
    email_verified :: Text,
    name :: Text,
    picture :: Text
  }

instance FromJSON GoogleTokenInfo where
  parseJSON = withObject "GoogleTokenInfo" $ \v ->
    GoogleTokenInfo
      <$> v .: "email"
      <*> v .: "email_verified"
      <*> v .: "name"
      <*> v .: "picture"

fetchTokenInfo :: Text -> IO (Either String GoogleTokenInfo)
fetchTokenInfo token = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest $ googleTokenInfoUrl token
  response <- httpLbs request manager
  let statusCode' = statusCode $ responseStatus response
  if statusCode' == 200
    then return $ eitherDecode $ responseBody response
    else return $ Left $ "Failed to fetch token info, status code: " ++ show statusCode'

postSocialLogin :: SL.SocialLoginReq -> Environment.Flow SL.SocialLoginRes
postSocialLogin req = do
  case req.oauthProvider of
    SL.Google -> do
      result <- L.runIO $ fetchTokenInfo req.tokenId
      case result of
        Right info -> do
          if info.email == req.email
            then do
              oldPerson <- PQ.findByEmail (Just req.email)
              person <-
                case oldPerson of
                  Just person' -> pure person'
                  Nothing -> do
                    deploymentVersion <- asks (.version)
                    moc <- CQMOC.findById req.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound req.merchantOperatingCityId.getId)
                    let createPersonInput = buildCreatePersonInput moc.city
                    DR.createDriverWithDetails createPersonInput Nothing Nothing Nothing Nothing (Just deploymentVersion.getDeploymentVersion) req.merchantId req.merchantOperatingCityId False
              QR.deleteByPersonId person.id
              token <- makeSession person.id.getId req.merchantId.getId req.merchantOperatingCityId.getId
              _ <- QR.create token
              pure $ SL.SocialLoginRes token.token
            else throwError . FailedToVerifyGoogleTokenId $ "token email: " <> show info.email <> ",  req.tokenId:" <> show req.tokenId <> ", req.email: " <> show req.email
        Left err -> throwError . FailedToVerifyGoogleTokenId $ req.tokenId <> ", error:" <> T.pack err
    SL.IOS -> throwError . InternalError $ "ios not implemented yet"
  where
    buildCreatePersonInput city =
      DR.CreatePersonInput
        { mobileNumber = Nothing,
          mobileCountryCode = Nothing,
          merchantId = req.merchantId.getId,
          merchantOperatingCity = Just city,
          email = Just req.email,
          identifierType = SP.EMAIL,
          registrationLat = req.registrationLat,
          registrationLon = req.registrationLon
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
