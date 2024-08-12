module Lib.Yudhishthira.Tools.Utils where

import Data.Aeson as A
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DTE
import qualified Data.Text.Lazy.Encoding as DTE
import JsonLogic
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import Lib.Yudhishthira.Storage.Queries.ChakraQueries as SQCQ
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ChakraQueries as LYT
import qualified Lib.Yudhishthira.Types.Common as C

toQueryResultInfo :: Text -> C.QueryResultInfo
toQueryResultInfo fieldName = C.QueryResultInfo fieldName C.Str

fromQueryResultInfo :: C.QueryResultInfo -> Text
fromQueryResultInfo (C.QueryResultInfo fieldName _) = fieldName

mandatoryChakraFields :: [Text]
mandatoryChakraFields = ["userId"]

getChakraQueryFields :: BeamFlow m r => LYT.Chakra -> m [Text]
getChakraQueryFields chakra = do
  queries <- SQCQ.findAllByChakra chakra
  return $ filter (\field -> field `notElem` mandatoryChakraFields) $ concatMap (map (\(C.QueryResultInfo fieldName _fieldType) -> fieldName) . (.queryResults)) queries

decodeTextToValue :: Text -> Either String Value
decodeTextToValue text =
  let byteString = DTE.encodeUtf8 $ DTE.fromStrict text
   in A.eitherDecode byteString

runJsonLogic :: (MonadFlow m) => Text -> Text -> m A.Value
runJsonLogic dataText ruleText = do
  let eitherRule = decodeTextToValue ruleText
  let eitherData = decodeTextToValue dataText
  rule <-
    case eitherRule of
      Right rule -> return rule
      Left err -> throwError $ InternalError ("Unable to decode rule:" <> show err)
  data' <-
    case eitherData of
      Right data_ -> return data_
      Left err -> throwError $ InternalError ("Unable to decode data:" <> show err)
  jsonLogic rule data'

getQueriesResult :: forall m. CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m => LYT.ChakraQueries -> m [(Text, Value)]
getQueriesResult LYT.ChakraQueries {..} = do
  qr :: Either String [String] <- CH.runRawQuery @CH.APP_SERVICE_CLICKHOUSE @[String] @m (Proxy @CH.APP_SERVICE_CLICKHOUSE) (CH.RawQuery $ DT.unpack queryText)
  do
    case qr of
      Left err -> throwError . InternalError $ DT.pack err
      Right qr' -> do
        let res = zip queryResults qr'
        mapM
          ( \(C.QueryResultInfo fieldName fieldType, fieldValueStr) -> do
              val <-
                case fieldType of
                  C.Num -> do
                    case readMaybe fieldValueStr of
                      Just fieldValue -> return $ A.Number fieldValue
                      Nothing -> throwError . InternalError $ "Could not parse field value: " <> DT.pack fieldValueStr <> " as number"
                  C.Str -> pure . A.String $ DT.pack fieldValueStr
                  C.Bool ->
                    case readMaybe fieldValueStr of
                      Just fieldValue -> return $ A.Bool fieldValue
                      Nothing -> throwError . InternalError $ "Could not parse field value: " <> DT.pack fieldValueStr <> " as bool"
              return (fieldName, val)
          )
          res
