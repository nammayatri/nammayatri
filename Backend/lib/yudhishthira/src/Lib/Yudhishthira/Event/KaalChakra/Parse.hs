module Lib.Yudhishthira.Event.KaalChakra.Parse
  ( parseQueryResult,
    mkDefaultUserDataMap,
    appendDefaultValues,
    DefaultDataMap (..),
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as A
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Yudhishthira.Tools.Utils
import qualified Lib.Yudhishthira.Types as Yudhishthira
import qualified Lib.Yudhishthira.Types.ChakraQueries

-- Chakra Query results parsing ---

-- if one field did not parsed, then we skip whole object, and insert default object later
parseQueryResult :: (Monad m, Log m) => Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries -> [A.Object] -> m [A.Object]
parseQueryResult chakraQueries objects = (catMaybes <$>) $
  forM objects $ \obj -> do
    let eParsedObject = parseQueryResultObject chakraQueries obj
    case eParsedObject of
      Left err -> logError err >> pure Nothing
      Right parsedObject -> pure (Just parsedObject)

parseQueryResultObject :: Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries -> A.Object -> Either Text A.Object
parseQueryResultObject Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries {queryName, queryResults} notParsedResult = do
  foldlM replaceParsedQueryResultField A.empty queryResults
  where
    replaceParsedQueryResultField :: A.Object -> Yudhishthira.QueryResult -> Either Text A.Object
    replaceParsedQueryResultField accResult Yudhishthira.QueryResult {resultName, resultDefault} = do
      let mbField = A.lookup (A.fromText resultName) notParsedResult
      let userId = fromMaybe "NotFound" $ A.lookup (A.fromText userIdField) notParsedResult -- NotFound will never appear
      case mbField of
        Just field -> do
          parsedField <- do
            left (\message -> "Could not parse chakra query result: userId: " <> show userId <> "; queryName: " <> queryName <> "; field name: " <> resultName <> "; value: " <> show field <> "; error: " <> message) $
              parseQueryResultField resultDefault field
          Right $ A.insert (A.fromText resultName) parsedField accResult
        Nothing -> Left $ "Field in chakra query results not found: userId: " <> show userId <> "; queryName: " <> queryName <> "; field name: " <> resultName

parseQueryResultField :: Yudhishthira.QueryResultDefault -> A.Value -> Either Text A.Value
parseQueryResultField (Yudhishthira.BOOL _) (A.String "true") = Right $ A.Bool True
parseQueryResultField (Yudhishthira.BOOL _) (A.String "false") = Right $ A.Bool False
parseQueryResultField (Yudhishthira.BOOL _) (A.String str) = A.toJSON <$> readEither @Text @Bool str
parseQueryResultField (Yudhishthira.BOOL _) (A.Number 1) = Right $ A.Bool True
parseQueryResultField (Yudhishthira.BOOL _) (A.Number 0) = Right $ A.Bool False
parseQueryResultField (Yudhishthira.BOOL _) (A.Number _) = Left "Could not parse BOOL value"
parseQueryResultField (Yudhishthira.BOOL _) (A.Bool b) = Right $ A.Bool b
parseQueryResultField (Yudhishthira.BOOL _) _ = Left "Only String, Number and Bool chakra query results supported"
parseQueryResultField (Yudhishthira.INT _) (A.String str) = A.toJSON <$> readEither @Text @Int str
parseQueryResultField (Yudhishthira.INT _) (A.Number n) = Right $ A.Number n -- TODO check that number is integer?
parseQueryResultField (Yudhishthira.INT _) _ = Left "Only String and Number chakra query results supported"
parseQueryResultField (Yudhishthira.DOUBLE _) (A.String str) = A.toJSON <$> readEither @Text @Double str
parseQueryResultField (Yudhishthira.DOUBLE _) (A.Number n) = Right $ A.Number n
parseQueryResultField (Yudhishthira.DOUBLE _) _ = Left "Only String and Number chakra query results supported"
parseQueryResultField (Yudhishthira.TEXT _) (A.String str) = Right $ A.String str
parseQueryResultField (Yudhishthira.TEXT _) _ = Left "Only String chakra query results supported"

-- Chakra Query default results ---

data DefaultDataMap = DefaultDataMap
  { queryName :: Text,
    defaultData :: A.Object
  }

mkDefaultUserDataMap :: [Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries] -> [DefaultDataMap]
mkDefaultUserDataMap = map $ \Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries {..} ->
  DefaultDataMap
    { queryName,
      defaultData = mkDefaultData queryResults
    }

mkDefaultData :: [Yudhishthira.QueryResult] -> A.Object
mkDefaultData = A.fromList . map (\queryResult -> A.fromText queryResult.resultName A..= mkDefaultDataField queryResult.resultDefault)

mkDefaultDataField :: Yudhishthira.QueryResultDefault -> A.Value
mkDefaultDataField (Yudhishthira.BOOL v) = A.toJSON v
mkDefaultDataField (Yudhishthira.INT v) = A.toJSON v
mkDefaultDataField (Yudhishthira.DOUBLE v) = A.toJSON v
mkDefaultDataField (Yudhishthira.TEXT v) = A.toJSON v

appendDefaultValues :: Id Yudhishthira.User -> A.Object -> [DefaultDataMap] -> (A.Object, [Text])
appendDefaultValues userId userDataObj = foldl appendDefaultValue (userDataObj, [])
  where
    appendDefaultValue :: (A.Object, [Text]) -> DefaultDataMap -> (A.Object, [Text])
    appendDefaultValue (obj, accQueries) DefaultDataMap {queryName, defaultData} = case A.lookup (A.fromText queryName) obj of
      Just _ -> (obj, accQueries)
      Nothing -> do
        let defaultUserData = A.Object $ A.insert (A.fromText userIdField) (A.String userId.getId) defaultData
        (A.insert (A.fromText queryName) defaultUserData obj, queryName : accQueries)
