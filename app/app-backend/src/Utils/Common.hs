{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Utils.Common where

import App.Types
import Beckn.Product.Validation.Context
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Types.Id
import Beckn.Types.Mobility.Intent
import Beckn.Types.Mobility.Payload
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Utils.Common as Utils
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.HeaderAuth
import Data.Text as DT
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant hiding (Context)
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import qualified Test.RandomStrings as RS
import qualified Types.API.Search as API
import Types.Error

-- | Performs simple token verification.
type TokenAuth = HeaderAuth "token" VerifyToken

data VerifyToken = VerifyToken

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (TokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = Person.Person
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

verifyPerson :: RegToken -> Flow Person.Person
verifyPerson token = do
  sr <- Utils.Common.verifyToken token
  Person.findById (Id $ SR._EntityId sr)
    >>= Utils.fromMaybeM PersonNotFound

verifyPersonAction :: VerificationAction VerifyToken AppEnv
verifyPersonAction = VerificationAction Utils.Common.verifyPerson

verifyToken :: RegToken -> Flow SR.RegistrationToken
verifyToken token =
  RegistrationToken.findByToken token
    >>= Utils.fromMaybeM InvalidToken
    >>= validateToken

validateToken :: SR.RegistrationToken -> Flow SR.RegistrationToken
validateToken sr@SR.RegistrationToken {..} = do
  let nominal = realToFrac $ _tokenExpiry * 24 * 60 * 60
  expired <- Utils.isExpired nominal _updatedAt
  when expired $ Utils.throwError TokenExpired
  return sr

generateShortId :: Flow Text
generateShortId = T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 10)

mkContext :: Text -> Text -> Text -> UTCTime -> Maybe BaseUrl -> Maybe BaseUrl -> Context
mkContext action txnId msgId utcTime bapUri bppUri =
  Context
    { _domain = MOBILITY,
      _country = Just "IND",
      _city = Nothing,
      _action = action,
      _core_version = Just "0.8.2",
      _domain_version = Just "0.8.2",
      _bap_uri = bapUri,
      _bpp_uri = bppUri,
      _transaction_id = txnId,
      _message_id = msgId,
      _timestamp = utcTime,
      _ttl = Nothing
    }

mkIntent :: API.SearchReq -> Intent
mkIntent req =
  Intent
    { _query_string = Nothing,
      _provider_id = Nothing,
      _category_id = Nothing,
      _item_id = Nothing,
      _tags = Nothing,
      _pickups = [toBeckn (req ^. #origin)],
      _drops = [toBeckn (req ^. #destination)],
      _vehicle = toBeckn $ req ^. #vehicle,
      _payload = Payload Nothing Nothing [] Nothing,
      _transfer = Nothing,
      _fare = toBeckn $ req ^. #fare
    }

validateContext :: Text -> Context -> Flow ()
validateContext action context = do
  validateDomain MOBILITY context
  validateContextCommons action context
