module Domain.Action.UI.ZendeskSdkToken (postProfileZendeskSdkToken) where

import qualified API.Types.UI.ZendeskSdkToken as Types
import qualified Data.Map.Strict as M
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Domain.Types.MerchantServiceConfig
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Utils.Common
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Interface.Types (getOneConfig)
import qualified Storage.Queries.Person as QPerson
import Tools.Auth (verifyPerson)
import Tools.Error
import Web.JWT (Algorithm (HS256), ClaimsMap (..), JOSEHeader (..), JWTClaimsSet (..), encodeSigned, hmacSecret, numericDate, stringOrURI)

postProfileZendeskSdkToken :: Types.ZendeskJwtReq -> Flow Types.ZendeskJwtResp
postProfileZendeskSdkToken req = do
  (personId, merchantId) <- verifyPerson req.user_token
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  merchantServiceConfig <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId, merchantId = merchantId.getId, serviceName = Just (IssueTicketService Ticket.Zendesk)})
      >>= fromMaybeM (InternalError "Zendesk service config not found for this merchant")
  zendeskCfg <- case merchantServiceConfig.serviceConfig of
    IssueTicketServiceConfig (Ticket.ZendeskConfig cfg) -> return cfg
    _ -> throwError $ InternalError "Unexpected Zendesk service config type"
  encJwtSecret <- zendeskCfg.jwtSecret & fromMaybeM (InternalError "Zendesk JWT secret not configured")
  jwtSecret <- decrypt encJwtSecret
  mbEmail <- mapM decrypt person.email
  now <- getCurrentTime
  let externalId = person.id.getId
      nowSecsInt = floor (utcTimeToPOSIXSeconds now) :: Int
      nowSecs = fromIntegral nowSecsInt
      expSecs = fromIntegral (nowSecsInt + 3600)
      jtiText = externalId <> "-" <> show nowSecsInt
      name = fromMaybe "NY User" $ do
        fn <- person.firstName
        ln <- person.lastName
        pure $ fn <> " " <> ln
      email = fromMaybe ("user_" <> externalId <> "@nammayatri.in") mbEmail
      claimsMap =
        M.fromList
          [ ("name", toJSON name),
            ("external_id", toJSON externalId),
            ("email", toJSON email),
            ("scope", toJSON ("user" :: Text))
          ]
      claims =
        JWTClaimsSet
          { iss = Nothing,
            sub = Nothing,
            aud = Nothing,
            exp = numericDate expSecs,
            nbf = Nothing,
            iat = numericDate nowSecs,
            jti = stringOrURI jtiText,
            unregisteredClaims = ClaimsMap claimsMap
          }
  mbMessagingKeyId <- mapM decrypt zendeskCfg.messagingKeyId
  let header =
        JOSEHeader
          { typ = Just "JWT",
            alg = Just HS256,
            cty = Nothing,
            kid = mbMessagingKeyId
          }
      key = hmacSecret jwtSecret
      signedJwt = encodeSigned key header claims
  return $ Types.ZendeskJwtResp {jwt = signedJwt}
