module Utils.Common where

import Beckn.Types.App
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common (withFlowHandler)
import qualified Beckn.Utils.Extra as Utils
import Beckn.Utils.Servant.Auth
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Time as DT
import Data.Time.Clock
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import qualified Test.RandomStrings as RS

-- | Performs simple token verification.
type TokenAuth = TokenAuth' VerifyToken

data VerifyToken = VerifyToken

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = SR.RegistrationToken
  verifyToken = Utils.Common.verifyToken
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

verifyToken :: RegToken -> L.Flow SR.RegistrationToken
verifyToken token =
  RegistrationToken.findByToken token
    >>= fromMaybeM400 "INVALID_TOKEN"
    >>= validateToken

validateToken :: SR.RegistrationToken -> L.Flow SR.RegistrationToken
validateToken sr@SR.RegistrationToken {..} = do
  let nominal = realToFrac $ _tokenExpiry * 24 * 60 * 60
  expired <- Utils.isExpired nominal _updatedAt
  when expired (L.throwException $ err400 {errBody = "TOKEN_EXPIRED"})
  return sr

fromMaybeM :: ServerError -> Maybe a -> L.Flow a
fromMaybeM err Nothing = L.throwException err
fromMaybeM _ (Just a) = return a

fromMaybeM400, fromMaybeM500, fromMaybeM503 :: BSL.ByteString -> Maybe a -> L.Flow a
fromMaybeM400 a = fromMaybeM (err400 {errBody = a})
fromMaybeM500 a = fromMaybeM (err500 {errBody = a})
fromMaybeM503 a = fromMaybeM (err503 {errBody = a})

generateShortId :: L.Flow Text
generateShortId = T.pack <$> (L.runIO $ RS.randomString (RS.onlyAlphaNum RS.randomASCII) 10)
