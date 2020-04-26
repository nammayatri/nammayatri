module Beckn.Utils.Storage where

import qualified EulerHS.Language as L
import EulerHS.Prelude

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
