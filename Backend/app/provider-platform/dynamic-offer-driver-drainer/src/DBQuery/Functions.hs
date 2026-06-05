module DBQuery.Functions where

import Config.Env (getDbConnectionRetryDelay, getDbConnectionRetryMaxAttempts)
import Control.Exception (throwIO)
import DBQuery.Types
import qualified Data.Map.Strict as M
import Data.Pool (Pool, destroyAllResources, withResource)
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as Pg
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as ET
import Text.Casing (quietSnake)

currentSchemaName :: String
currentSchemaName = "atlas_driver_offer_bpp"

generateInsertQuery :: InsertQuery -> Maybe Text
generateInsertQuery InsertQuery {..} = do
  let schemaName = schema.getSchemaName
  if null termWarps
    then Nothing
    else do
      let (columnNames, values) =
            unzip $
              termWarps <&> \(TermWrap column value) -> do
                let keyText = quote' $ replaceMappings column mappings
                let valueText = valueToText value
                (keyText, valueText)
          table = schemaName <> "." <> quote' (textToSnakeCaseText dbModel.getDBModel)
          inserts = T.intercalate ", " columnNames
          valuesList = T.intercalate ", " values
      Just $ "INSERT INTO " <> table <> " (" <> inserts <> ") VALUES (" <> valuesList <> ")" <> " ON CONFLICT DO NOTHING;"

generateUpdateQuery :: UpdateQuery -> Maybe Text
generateUpdateQuery UpdateQuery {..} = do
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
      let correctSetClauseText = map (\(Set column value) -> (replaceMappings column mappings, valueToText value)) setClauses
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
