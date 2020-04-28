module Beckn.Utils.Storage where

import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Storage.Queries.RegistrationToken as QR

import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant

data AppException
  = SqlDBConnectionFailedException Text
  | KVDBConnectionFailedException Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Exception)

throwOnFailedWithLog ::
     Show e => Either e a -> (Text -> AppException) -> Text -> L.Flow ()
throwOnFailedWithLog (Left err) mkException msg = do
  L.logError ("" :: Text) $ msg <> " " <> show err <> ""
  L.throwException $ mkException $ msg <> " " <> show err <> ""
throwOnFailedWithLog _ _ _ = pure ()

throwFailedWithLog :: (Text -> AppException) -> Text -> L.Flow ()
throwFailedWithLog mkException msg = do
  L.logError ("" :: Text) $ msg <> ""
  L.throwException $ mkException $ msg <> ""

verifyToken :: Maybe Text -> L.Flow SR.RegistrationToken
verifyToken (Just token) =
  QR.findRegistrationToken token >>=
    maybe (L.throwException $ err400 {errBody = "INVALID_TOKEN"}) pure
verifyToken _ =
  L.throwException $ err400 {errBody = "NO_TOKEN_FOUND"}
