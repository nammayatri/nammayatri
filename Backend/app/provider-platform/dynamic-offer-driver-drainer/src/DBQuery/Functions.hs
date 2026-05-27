module DBQuery.Functions where

import Config.Env (getDbConnectionRetryDelay, getDbConnectionRetryMaxAttempts)
import Control.Exception (throwIO)
import DBQuery.Types
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import Data.Pool (Pool, destroyAllResources, withResource)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.PostgreSQL.Simple as Pg
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as ET
import Text.Casing (quietSnake)

currentSchemaName :: String
currentSchemaName = "atlas_driver_offer_bpp"

generateInsertQuery :: [Text] -> InsertQuery -> Maybe Text
generateInsertQuery jsonRepairModels InsertQuery {..} = do
  let schemaName = schema.getSchemaName
  if null termWarps
    then Nothing
    else do
      let (columnNames, values) =
            unzip $
              termWarps <&> \(TermWrap column value) -> do
                let keyText = quote' $ replaceMappings column mappings
                let valueText = valueToTextForModel jsonRepairModels dbModel value
                (keyText, valueText)
          table = schemaName <> "." <> quote' (textToSnakeCaseText dbModel.getDBModel)
          inserts = T.intercalate ", " columnNames
          valuesList = T.intercalate ", " values
      Just $ "INSERT INTO " <> table <> " (" <> inserts <> ") VALUES (" <> valuesList <> ")" <> " ON CONFLICT DO NOTHING;"

generateUpdateQuery :: [Text] -> UpdateQuery -> Maybe Text
generateUpdateQuery jsonRepairModels UpdateQuery {..} = do
  let schemaName = schema.getSchemaName
  let correctWhereClauseText = makeWhereCondition whereClause mappings
      setQuery = makeSetConditions
      table = schemaName <> "." <> quote' (textToSnakeCaseText dbModel.getDBModel)
  if T.null correctWhereClauseText
    then Nothing
    else Just $ "UPDATE " <> table <> " SET " <> setQuery <> " WHERE " <> correctWhereClauseText <> ";"
  where
    makeSetConditions :: Text
    makeSetConditions = do
      let correctSetClauseText = map (\(Set column value) -> (replaceMappings column mappings, valueToTextForModel jsonRepairModels dbModel value)) setClauses
      T.intercalate "," (map (\(k, v) -> (quote' . textToSnakeCaseText) k <> "=" <> v) correctSetClauseText)

generateDeleteQuery :: DeleteQuery -> Maybe Text
generateDeleteQuery DeleteQuery {..} = do
  let schemaName = schema.getSchemaName
      correctWhereClauseText = makeWhereCondition whereClause mappings
      table = schemaName <> "." <> quote' (textToSnakeCaseText dbModel.getDBModel)
  if T.null correctWhereClauseText
    then Nothing
    else Just $ "DELETE FROM " <> table <> " WHERE " <> correctWhereClauseText <> ";"

executeQuery :: Pg.Connection -> Pg.Query -> IO ()
executeQuery conn query' = do
  result <- try $ Pg.execute_ conn query' :: IO (Either SomeException Int64)
  case result of
    Left e -> throwIO $ QueryError $ "Query execution failed: " <> T.pack (show e)
    Right _ -> return ()

executeQueryUsingConnectionPool :: Pool Pg.Connection -> Pg.Query -> IO ()
executeQueryUsingConnectionPool pool query' = do
  res <- try $ withResource pool $ \conn -> Pg.execute_ conn query'
  case res of
    Left (e :: SomeException) ->
      if isConnectionError e || isConnectionError' e
        then do
          putStrLn @String "[Failover] Destroying all pool connections to handle potential database failover"
          destroyAllResources pool
          maxAttempts <- getDbConnectionRetryMaxAttempts
          retryDelay <- getDbConnectionRetryDelay
          executeQueryWithRetry pool query' maxAttempts retryDelay e
        else throwIO $ QueryError $ "Query execution failed: " <> T.pack (show e)
    Right _ -> return ()

executeQueryWithRetry :: Pool Pg.Connection -> Pg.Query -> Int -> Int -> SomeException -> IO ()
executeQueryWithRetry pool query' maxAttempts retryDelay firstError = go (maxAttempts - 1) firstError
  where
    go attemptsLeft lastError = do
      if attemptsLeft > 0
        then do
          let currentAttempt = maxAttempts - attemptsLeft
          let backoffDelay = retryDelay * currentAttempt
          putStrLn @String $ "[Retry] Database connection error - Error: " ++ show lastError ++ " - Attempts left: " ++ show attemptsLeft ++ " - Retry delay: " ++ show (backoffDelay `div` 1000000) ++ " seconds"
          threadDelay backoffDelay
          res <- try $ withResource pool $ \conn -> Pg.execute_ conn query'
          case res of
            Left (e :: SomeException) ->
              if isConnectionError e || isConnectionError' e
                then go (attemptsLeft - 1) e
                else throwIO $ QueryError $ "Query execution failed: " <> T.pack (show e)
            Right _ -> return ()
        else
          throwIO $
            QueryError $
              T.pack $
                "Query execution failed after " ++ show maxAttempts ++ " attempts. Last error: " ++ show lastError

isConnectionError :: SomeException -> Bool
isConnectionError e =
  let res = transformException e
   in case res of
        ET.DBError (ET.SQLError (ET.PostgresError (ET.PostgresSqlError "" ET.PostgresFatalError "" "" ""))) _ -> True
        ET.DBError (ET.SQLError (ET.PostgresError (ET.PostgresSqlError "25006" ET.PostgresFatalError _ _ _))) _ -> True
        _ -> False

isConnectionError' :: SomeException -> Bool
isConnectionError' e =
  let errorMsg = T.toLower $ T.pack $ show e
   in any (`T.isInfixOf` errorMsg) connectionErrorPatterns
  where
    connectionErrorPatterns =
      [ "server closed the connection",
        "server terminated abnormally",
        "connection to server",
        "timeout",
        "network",
        "host is unreachable",
        "no route to host",
        "connection reset by peer",
        "broken pipe",
        "connection timed out",
        "connection refused",
        "name resolution failed",
        "connection closed",
        "cannot execute insert in a read-only transaction",
        "read-only transaction",
        "sqlstate = \"25006\""
      ]

transformException :: SomeException -> ET.DBError
transformException e =
  maybe
    (ET.DBError ET.UnrecognizedError $ show e)
    (ET.postgresErrorToDbError (show e))
    $ fromException e

textToSnakeCaseText :: Text -> Text
textToSnakeCaseText = T.pack . quietSnake . T.unpack

-- | We are setting mappings in case of beamColumn name is different from the Db column name
replaceMappings :: Column -> Mapping -> Text
replaceMappings (Column element) (Mapping obj) =
  case M.lookup element obj of
    Just value -> value
    Nothing -> textToSnakeCaseText element

quote' :: Text -> Text
quote' t = "\"" <> t <> "\""

quote :: Text -> Text
quote t = "'" <> T.replace "'" "''" t <> "'"

-- for "contents_v2" field
valueToText :: Value -> T.Text
valueToText value = case value of
  SqlNull -> "null"
  SqlString t -> quote t
  SqlInteger n -> show n
  SqlNum n -> show n
  SqlValue t -> quote t
  SqlList a -> quote $ "{" <> T.intercalate "," (map valueToText' a) <> "}" -- in case of array of value of a key in object
  where
    -- we should escape symbols inside of list, so use 'show' instead of just double quotes suffix and prefix
    valueToText' :: Value -> T.Text
    valueToText' SqlNull = "null"
    valueToText' (SqlString t) = show t -- quote' t
    valueToText' (SqlNum n) = show n
    valueToText' (SqlInteger n) = show n
    valueToText' (SqlValue t) = show t -- quote' t
    valueToText' (SqlList a) = "[" <> T.intercalate "," (map valueToText' a) <> "]"

-- | Like 'valueToText', but for the scoped models (configured via Dhall
-- @jsonRepairModels@) it first tries to repair a value that was accidentally
-- serialized via Haskell's `Show` instance for an Aeson Value (e.g.
-- "Object (fromList [])") back into valid JSON ("{}"). Without this, such values
-- break INSERT/UPDATE on json columns with `invalid input syntax for type json`.
-- Non-scoped models are untouched. The model list is an interim drainer-side
-- mitigation for poison stream entries while the producer-side fix (correct
-- `ToSQLObject` instance) rolls out.
valueToTextForModel :: [Text] -> DBModel -> Value -> T.Text
valueToTextForModel jsonRepairModels dbModel value
  | dbModel.getDBModel `elem` jsonRepairModels =
    case value of
      -- The producer quote-wraps every column value (via `show`), so the
      -- poison `Object (fromList [])` lands in the `SqlString` branch, not
      -- `SqlValue`. We attempt repair on both; the prefix-gate in
      -- 'repairShownAesonValue' leaves ordinary strings (e.g. "PENDING") alone.
      SqlString t -> quote $ fromMaybe t (repairShownAesonValue t)
      SqlValue t -> quote $ fromMaybe t (repairShownAesonValue t)
      _ -> valueToText value
  | otherwise = valueToText value

-- | Repair the textual output of Haskell's `Show` for a @Data.Aeson.Value@ back
-- into JSON text. Aeson's @Read Value@ instance round-trips its @Show@, so we
-- read the value and re-encode it. Only the container show-forms
-- ("Object (fromList " / "Array ") are treated as repairable, so legitimate
-- enum/text values (e.g. "PENDING", "Null") are never touched. Returns Nothing
-- when the text is not a recognizable shown-Aeson container or cannot be
-- parsed, in which case the caller keeps the original text.
repairShownAesonValue :: Text -> Maybe Text
repairShownAesonValue t =
  let s = T.unpack (T.stripStart t)
   in if "Object (fromList " `isPrefixOf` s || "Array " `isPrefixOf` s
        then (\v -> TE.decodeUtf8 $ LBS.toStrict $ A.encode v) <$> (readMaybe s :: Maybe A.Value)
        else Nothing

valueToTextForInConditions :: [Value] -> T.Text
valueToTextForInConditions values = "(" <> T.intercalate "," (map valueToText values) <> ")"

makeWhereCondition :: Where -> Mapping -> Text
makeWhereCondition whereClause mappings = do
  case whereClause of
    [] -> "true" -- TODO test this
    [clause] -> makeClauseCondition clause
    clauses -> makeClauseCondition (And clauses)
  where
    makeClauseCondition :: Clause -> Text
    makeClauseCondition clause = do
      case clause of
        And clauses -> getArrayConditionText clauses " AND " mappings
        Or clauses -> getArrayConditionText clauses " OR " mappings
        Is column term -> makeTermCondition column term

    makeTermCondition :: Column -> Term -> Text
    makeTermCondition column term = do
      let columnText = quote' $ replaceMappings column mappings
      case term of
        In values -> columnText <> " IN " <> valueToTextForInConditions values
        Eq value -> columnText <> " = " <> valueToText value
        GreaterThan value -> columnText <> " > " <> valueToText value
        GreaterThanOrEq value -> columnText <> " >= " <> valueToText value
        LessThan value -> columnText <> " < " <> valueToText value
        LessThanOrEq value -> columnText <> " <= " <> valueToText value
        Null -> columnText <> " IS NULL"
        Like txt -> columnText <> " LIKE " <> txt
        Not (Eq value) -> columnText <> " != " <> valueToText value
        Not (In values) -> columnText <> " NOT IN " <> valueToTextForInConditions values
        Not Null -> columnText <> " IS NOT NULL"
        Not term' -> " NOT " <> "(" <> makeTermCondition column term' <> ")"

getArrayConditionText :: [Clause] -> Text -> Mapping -> Text
getArrayConditionText clauses cnd mappings = case clauses of
  [] -> "true"
  [x] -> makeWhereCondition [x] mappings
  (x : xs) -> "(" <> makeWhereCondition [x] mappings <> ")" <> cnd <> "(" <> getArrayConditionText xs cnd mappings <> ")"
