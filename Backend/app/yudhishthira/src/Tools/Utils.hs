module Tools.Utils where

import qualified Data.Aeson as A
import qualified Data.Text.Lazy as DTE
import qualified Data.Text.Lazy.Encoding as DTE
import qualified Environment
import JsonLogic
import Kernel.Prelude
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Queries.ChakraQueries as SQCQ

mandatoryChakraFields :: [Text]
mandatoryChakraFields = ["userId"]

getChakraQueryFields :: LYT.Chakra -> Environment.Flow [Text]
getChakraQueryFields chakra = do
  queries <- SQCQ.findAllByChakra chakra
  return $ filter (\field -> field `notElem` mandatoryChakraFields) $ concatMap (.queryResults) queries

runJsonLogic :: Text -> Text -> Environment.Flow A.Value
runJsonLogic ruleText dataText = do
  let eitherRule = A.eitherDecode . DTE.fromStrict . DTE.encodeUtf8 $ ruleText
  let eitherData = A.eitherDecode . DTE.fromStrict . DTE.encodeUtf8 $ dataText
  let rule =
        case eitherRule of
          Right rule -> return rule
          Left err -> throwError $ InternalError ("Unable to decode rule:" <> show err)
  let data' =
        case eitherData of
          Right data_ -> return data_
          Left err -> throwError $ InternalError ("Unable to decode data:" <> show err)
  jsonLogic rule data'
