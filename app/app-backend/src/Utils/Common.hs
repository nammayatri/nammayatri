module Utils.Common where

import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Time as DT
import Data.Time.Clock
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import qualified Test.RandomStrings as RS

verifyToken :: Maybe Text -> L.Flow SR.RegistrationToken
verifyToken (Just token) =
  RegistrationToken.findByToken token
    >>= fromMaybeM400 "INVALID_TOKEN"
    >>= validateToken
verifyToken _ = L.throwException $ err400 {errBody = "NO_TOKEN_FOUND"}

validateToken :: SR.RegistrationToken -> L.Flow SR.RegistrationToken
validateToken sr@SR.RegistrationToken {..} = do
  let nominal = realToFrac $ _tokenExpiry * 24 * 60 * 60
  expired <- isExpired nominal _updatedAt
  when expired (L.throwException $ err400 {errBody = "TOKEN_EXPIRED"})
  return sr

fromMaybeM :: ServerError -> Maybe a -> L.Flow a
fromMaybeM err Nothing = L.throwException err
fromMaybeM _ (Just a) = return a

fromMaybeM400, fromMaybeM500, fromMaybeM503 :: BSL.ByteString -> Maybe a -> L.Flow a
fromMaybeM400 a = fromMaybeM (err400 {errBody = a})
fromMaybeM500 a = fromMaybeM (err500 {errBody = a})
fromMaybeM503 a = fromMaybeM (err503 {errBody = a})

isExpired :: DT.NominalDiffTime -> LocalTime -> L.Flow Bool
isExpired nominal time = do
  now <- getCurrTime
  let addedLocalTime = DT.addLocalTime nominal time
  return $ now > addedLocalTime

getCurrTime :: L.Flow LocalTime
getCurrTime = L.runIO $ do
  utc <- getCurrentTime
  timezone <- getTimeZone utc
  pure $ utcToLocalTime timezone utc

generateShortId :: L.Flow Text
generateShortId = T.pack <$> (L.runIO $ RS.randomString (RS.onlyAlphaNum RS.randomASCII) 10)
