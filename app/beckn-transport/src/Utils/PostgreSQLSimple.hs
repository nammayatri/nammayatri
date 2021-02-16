{-# LANGUAGE TypeApplications #-}

module Utils.PostgreSQLSimple (postgreSQLSimpleExecute, postgreSQLSimpleQuery) where

import App.Types
import Beckn.Storage.DB.Config (DBConfig (..))
import qualified Beckn.Storage.Queries as DB
import Beckn.Utils.Common
import Control.Exception (Handler (..), catches)
import Data.Aeson
import Data.Pool (withResource)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (decodeUtf8, encodeUtf8, pack, (.=))
import EulerHS.Types (SqlConn (PostgresPool), mkPostgresPoolConfig)

postgreSQLSimpleExecute :: ToRow row => Query -> row -> Flow Int64
postgreSQLSimpleExecute q qargs = do
  logQuery q qargs
  withPostgreSQLSimple (\conn -> execute conn q qargs)

postgreSQLSimpleQuery ::
  ( FromJSON res,
    ToJSON res,
    FromRow res,
    ToRow row
  ) =>
  Query ->
  row ->
  Flow [res]
postgreSQLSimpleQuery q qargs = do
  logQuery q qargs
  withPostgreSQLSimple (\conn -> query conn q qargs)

withPostgreSQLSimple :: (FromJSON a, ToJSON a) => (Connection -> IO a) -> Flow a
withPostgreSQLSimple f = do
  DBConfig {..} <- asks dbCfg
  pool <-
    L.getSqlDBConnection (mkPostgresPoolConfig connTag pgConfig poolConfig)
      >>= either DB.throwDBError pure
      >>= \case
        PostgresPool _connTag pool -> pure pool
        _ -> throwError500 "NOT_POSTGRES_BACKEND"
  Either' res <-
    L.runIO
      . withResource pool
      $ fmap Either'
        . runPostgresqlSimple
        . fmap Right
        . f
  either throwError500 pure res

runPostgresqlSimple :: IO (Either Text a) -> IO (Either Text a)
runPostgresqlSimple = (`catches` handlers)
  where
    handlers =
      [ Handler (\(e :: FormatError) -> pure . Left . pack $ fmtMessage e),
        Handler (\(e :: ResultError) -> pure . Left . pack $ showResultError e),
        Handler (\(e :: QueryError) -> pure . Left . pack $ qeMessage e),
        Handler
          ( \(e :: SqlError) ->
              pure . Left . decodeUtf8 $
                sqlErrorMsg e <> " (" <> sqlErrorDetail e <> ") (" <> sqlErrorHint e <> ")"
          )
      ]

showResultError :: ResultError -> String
showResultError (Incompatible sqlType _ _ hType msg) = "sql incompatible: " <> msg <> " (" <> hType <> " ~ " <> sqlType <> ")"
showResultError (UnexpectedNull _ _ field _ msg) = "sql unexpected null: " <> msg <> " @ " <> field
showResultError (ConversionFailed sqlType _ _ hType msg) = "sql conversion failed" <> msg <> " (" <> hType <> " ~ " <> sqlType <> ")"

logQuery :: ToRow q => Query -> q -> ReaderT AppEnv L.Flow ()
logQuery q qargs =
  withPostgreSQLSimple (\conn -> decodeUtf8 <$> formatQuery conn q qargs)
    >>= L.logDebug @Text "raw sql query" -- TODO: replace with Beckn.Utils.Logging.logDebug

-- Need this for L.runIO
newtype Either' a b = Either' (Either a b)

instance (FromJSON a, FromJSON b) => FromJSON (Either' a b) where
  parseJSON = withObject "Either'" $ \o ->
    fmap Either' $
      (Right <$> o .: "Right")
        <|> (Left <$> o .: "Left")

instance (ToJSON a, ToJSON b) => ToJSON (Either' a b) where
  toJSON (Either' ei) = either (f "Left") (f "Right") ei
    where
      f field val = object [field .= val]
