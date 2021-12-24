module Beckn.Storage.DB.Utils where

import qualified Data.Text as T
import Database.Beam.Backend.SQL
  ( FromBackendRow (fromBackendRow),
    FromBackendRowM,
  )
import EulerHS.Prelude

fromBackendRowEnum :: (Read a, FromBackendRow backend T.Text) => String -> FromBackendRowM backend a
fromBackendRowEnum typeName = do
  str <- T.unpack <$> fromBackendRow
  case readMaybe str of
    Nothing -> fail $ "failed to parse " ++ typeName ++ "; invalid value: " ++ str
    Just val -> pure val
