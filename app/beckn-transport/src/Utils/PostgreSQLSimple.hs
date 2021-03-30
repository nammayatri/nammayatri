{-# LANGUAGE TypeApplications #-}

module Utils.PostgreSQLSimple (postgreSQLSimpleExecute, postgreSQLSimpleQuery) where

import App.Types
import Beckn.Storage.DB.Config (DBConfig (..))
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
import Types.Error

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
      >>= either throwDBError pure
      >>= \case
        PostgresPool _connTag pool -> pure pool
        _ -> throwError NotPostgresBackend
  res <-
    L.runIO . withResource pool $
      runPostgresqlSimple . fmap Right . f
  either (throwErrorWithInfo SQLRequestError) pure res

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

logQuery :: ToRow q => Query -> q -> Flow ()
logQuery q qargs =
  withPostgreSQLSimple (\conn -> decodeUtf8 <$> formatQuery conn q qargs)
    >>= logDebug "raw sql query"
