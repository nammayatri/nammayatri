module Domain.Action.UI.ZendeskSdkToken (getProfileZendeskSdkToken) where

import qualified Data.Map.Strict as M
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Domain.Types.Merchant
import Domain.Types.MerchantServiceConfig
import Domain.Types.Person
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import Web.JWT (Algorithm (HS256), ClaimsMap (..), JOSEHeader (..), JWTClaimsSet (..), encodeSigned, hmacSecret, numericDate, stringOrURI)

getProfileZendeskSdkToken :: (Maybe (Id Person), Id Merchant) -> Flow Text
getProfileZendeskSdkToken (mbPersonId, merchantId) = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  merchantServiceConfig <-
    CQMSC.findByMerchantOpCityIdAndService merchantId person.merchantOperatingCityId (IssueTicketService Ticket.Zendesk)
      >>= fromMaybeM (InternalError "Zendesk service config not found for this merchant")
  zendeskCfg <- case merchantServiceConfig.serviceConfig of
    IssueTicketServiceConfig (Ticket.ZendeskConfig cfg) -> return cfg
    _ -> throwError $ InternalError "Unexpected Zendesk service config type"
  encJwtSecret <- zendeskCfg.jwtSecret & fromMaybeM (InternalError "Zendesk JWT secret not configured")
  jwtSecret <- decrypt encJwtSecret
  mbEmail <- mapM decrypt person.email
  now <- getCurrentTime
  let externalId = person.id.getId
      jtiText = externalId <> "-" <> (show . (round :: POSIXTime -> Int) $ utcTimeToPOSIXSeconds now)
      name = fromMaybe "NY User" $ do
        fn <- person.firstName
        ln <- person.lastName
        pure $ fn <> " " <> ln
      expTime = addUTCTime (secondsToNominalDiffTime 3600) now
      claimsMap =
        M.fromList $
          [ ("name", toJSON name),
            ("external_id", toJSON externalId)
          ]
            <> maybe [] (\email -> [("email", toJSON email)]) mbEmail
      claims =
        JWTClaimsSet
          { iss = Nothing,
            sub = Nothing,
            aud = Nothing,
            exp = numericDate $ utcTimeToPOSIXSeconds expTime,
            nbf = Nothing,
            iat = numericDate $ utcTimeToPOSIXSeconds now,
            jti = stringOrURI jtiText,
            unregisteredClaims = ClaimsMap claimsMap
          }
      header =
        JOSEHeader
          { typ = Just "JWT",
            alg = Just HS256,
            cty = Nothing,
            kid = Nothing
          }
      key = hmacSecret jwtSecret
  return $ encodeSigned key header claims
