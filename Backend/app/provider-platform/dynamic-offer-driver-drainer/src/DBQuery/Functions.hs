module DBQuery.Functions where

import Control.Exception (throwIO)
import DBQuery.Types
import qualified Data.Map.Strict as M
import Data.Pool (Pool, withResource)
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as Pg
import EulerHS.Prelude hiding (id)
import System.Posix.Signals (raiseSignal, sigKILL)
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
    Left (e :: SomeException) -> do
      errorText <- handleSqlError e
      throwIO $ QueryError errorText
    Right _ -> return ()

handleSqlError :: SomeException -> IO T.Text
handleSqlError e = case fromException e of
  Just (sqlError :: Pg.SqlError) -> return $ "Query execution failed: SqlError => " <> T.pack (show sqlError)
  _ -> do
    let errorString = show e
    if "libpq: failed" `T.isInfixOf` T.pack errorString
      then do
        raiseSignal sigKILL
        return $ "Query execution failed: " <> T.pack errorString
      else return $ "Query execution failed: " <> T.pack errorString

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
