module Lib.Yudhishthira.Event.KaalChakra (kaalChakraEvent, runQueryRequest, Handle (..)) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.Aeson.Types as A
import Data.List (nub)
import qualified Data.Map as M
import Data.Scientific (toRealFloat)
import qualified Data.Text as T
import qualified JsonLogic
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import Lib.Yudhishthira.Storage.Queries.ChakraQueries as QChakraQueries
import qualified Lib.Yudhishthira.Storage.Queries.NammaTag as QNammaTag
import qualified Lib.Yudhishthira.Storage.Queries.UserData as QUserData
import Lib.Yudhishthira.Tools.Error
import Lib.Yudhishthira.Tools.Utils
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.ChakraQueries
import qualified Lib.Yudhishthira.Types.NammaTag as DNT
import qualified Lib.Yudhishthira.Types.UserData as DUserData

data Handle m = Handle
  { getUserTags :: Id Lib.Yudhishthira.Types.User -> m (Maybe [Text]), -- Nothing if user not found
    updateUserTags :: Id Lib.Yudhishthira.Types.User -> [Text] -> m ()
  }

kaalChakraEvent ::
  (BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) =>
  Handle m ->
  Lib.Yudhishthira.Types.Chakra ->
  m ()
kaalChakraEvent h chakra = do
  chakraQueries <- QChakraQueries.findAllByChakra chakra
  chakraQueriesResults <- forM chakraQueries $ \chakraQuery -> do
    queryResult <- runQueryRequest chakraQuery
    pure (chakraQuery.queryName, queryResult)
  userDataList <- buildUserDataList chakra chakraQueriesResults
  QUserData.createMany userDataList
  tags <- QNammaTag.findAllByChakra chakra

  -- Skip LLM tags instead of throwing error
  filteredTags <- flip filterM tags $ \tag -> case tag.rule of
    Lib.Yudhishthira.Types.RuleEngine _ -> pure True
    Lib.Yudhishthira.Types.LLM _ -> do
      logError $ "LLM is not implemented: tag: " <> tag.name <> "; skipping."
      pure False

  forM_ userDataList $ \userData -> do
    let eTagValuesTuple = applyRule userData <$> filteredTags
    -- Skip current tag for current user instead of throwing error
    tagValuesTuple <- (catMaybes <$>) $
      forM eTagValuesTuple $ \case
        Right tagValueTuple -> pure $ Just tagValueTuple
        Left err -> logError err >> pure Nothing

    case tagValuesTuple of
      [] -> logDebug $ "No tags should be applied for current user: " <> show userData.userId
      _ -> do
        mbOldTagsText <- h.getUserTags userData.userId
        -- Skip current user instead of throwing error
        case updateUserTagValues userData.userId tagValuesTuple mbOldTagsText of
          Right updTagsText -> do
            if Just updTagsText /= mbOldTagsText
              then h.updateUserTags userData.userId updTagsText
              else logDebug $ "Tags did not changed for current user: " <> show userData.userId
          Left err -> logError err

applyRule ::
  DUserData.UserData ->
  DNT.NammaTag ->
  Either Text (Lib.Yudhishthira.Types.TagName, Lib.Yudhishthira.Types.TagValue)
applyRule userData tag = case tag.rule of
  Lib.Yudhishthira.Types.RuleEngine logic -> do
    let eTagValueObj = JsonLogic.jsonLogicEither logic userData.userDataValue
    case eTagValueObj of
      Left err -> do
        Left $ "Could not apply rule to data: userId: " <> show userData.userId <> "; tag: " <> show tag.name <> "; data: " <> show userData.userDataValue <> "; error: " <> show err <> "; skipping."
      Right tagValueObj -> do
        case parseTagValue tagValueObj tag.possibleValues of
          Nothing -> Left $ "Value is not allowed: " <> show tagValueObj <> "; userId: " <> show userData.userId <> "; tag: " <> tag.name <> "; skipping."
          Just tagValue -> Right (Lib.Yudhishthira.Types.TagName tag.name, tagValue)
  Lib.Yudhishthira.Types.LLM _ -> Left $ "LLM is not implemented: tag: " <> tag.name <> "; skipping."

updateUserTagValues ::
  Id Lib.Yudhishthira.Types.User ->
  [(Lib.Yudhishthira.Types.TagName, Lib.Yudhishthira.Types.TagValue)] ->
  Maybe [Text] ->
  Either Text [Text]
updateUserTagValues userId _ Nothing = Left $ "User with userId: " <> show userId <> " did not found; skipping."
updateUserTagValues userId updatedTags (Just oldTagsText) = do
  oldTagsMap <- (M.fromList <$>) $
    forM oldTagsText $ \oldTagText -> do
      case parseTagName oldTagText of
        Nothing -> Left $ "Could not parse tag name: " <> oldTagText <> "; userId: " <> show userId <> "; skipping."
        Just oldTagName -> Right (oldTagName, oldTagText)
  let updTagsMap = foldl (\tagsMap (tagName, tagValue) -> M.insert tagName (showTag tagName tagValue) tagsMap) oldTagsMap updatedTags
  Right $ snd <$> M.toList updTagsMap

parseTagName :: Text -> Maybe Lib.Yudhishthira.Types.TagName
parseTagName txt = case T.splitOn "#" txt of
  (tagName : _) -> Just (Lib.Yudhishthira.Types.TagName tagName)
  _ -> Nothing

showTag ::
  Lib.Yudhishthira.Types.TagName ->
  Lib.Yudhishthira.Types.TagValue ->
  Text
showTag (Lib.Yudhishthira.Types.TagName tagName) (Lib.Yudhishthira.Types.TextValue tagValueText) = tagName <> "#" <> tagValueText
showTag (Lib.Yudhishthira.Types.TagName tagName) (Lib.Yudhishthira.Types.NumberValue tagValueDouble) = tagName <> "#" <> show tagValueDouble

parseTagValue :: A.Value -> Lib.Yudhishthira.Types.TagValues -> Maybe Lib.Yudhishthira.Types.TagValue
parseTagValue (A.String txt) possibleValues = case possibleValues of
  Lib.Yudhishthira.Types.Tags possibleTxt | txt `elem` possibleTxt -> Just $ Lib.Yudhishthira.Types.TextValue txt
  Lib.Yudhishthira.Types.Tags _ -> Nothing
  Lib.Yudhishthira.Types.AnyText -> Just $ Lib.Yudhishthira.Types.TextValue txt
  Lib.Yudhishthira.Types.Range minDouble maxDouble -> case readMaybe @Double (T.unpack txt) of
    Just d | d >= minDouble && d <= maxDouble -> Just $ Lib.Yudhishthira.Types.NumberValue d
    _ -> Nothing
parseTagValue (A.Number sci) possibleValues = do
  let d = toRealFloat sci
  case possibleValues of
    Lib.Yudhishthira.Types.Range minDouble maxDouble | d >= minDouble && d <= maxDouble -> Just $ Lib.Yudhishthira.Types.NumberValue d
    _ -> Nothing
parseTagValue _ _ = Nothing

buildUserDataList ::
  (MonadFlow m) =>
  Lib.Yudhishthira.Types.Chakra ->
  [(Text, [A.Object])] ->
  m [DUserData.UserData]
buildUserDataList chakra chakraQueriesResults = do
  queryResultsMapping :: [(Id Lib.Yudhishthira.Types.User, Text, A.Object)] <-
    (concat <$>) $
      forM chakraQueriesResults $ \(queryName, chakraQueryResult) -> do
        forM chakraQueryResult $ \chakraQueryResultForUser -> do
          userId <- parseUserId chakraQueryResultForUser
          pure (userId, queryName, chakraQueryResultForUser)
  let userIds = nub $ map (\(userId, _, _) -> userId) queryResultsMapping

  forM userIds $ \userId -> do
    let queryResultsForUser = filter (\(userId', _, _) -> userId' == userId) queryResultsMapping
    let userDataValue = A.Object $ A.fromList $ queryResultsForUser <&> \(_userId, queryName, obj) -> A.fromText queryName A..= obj
    id <- generateGUID
    now <- getCurrentTime
    pure $ DUserData.UserData {chakra, id, userDataValue, userId, createdAt = now, updatedAt = now}

parseUserId :: (MonadThrow m, Log m) => A.Object -> m (Id Lib.Yudhishthira.Types.User)
parseUserId obj = do
  let mbUserId = flip A.parseMaybe obj $ \obj' ->
        obj' A..: A.fromText userIdField
  mbUserId & fromMaybeM (InternalError $ "userId not found in chakra query response: " <> show obj)

runQueryRequest ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries ->
  m [A.Object]
runQueryRequest queryRequest = do
  eQueryResult :: Either String [A.Object] <- CH.runRawQuery (Proxy @CH.APP_SERVICE_CLICKHOUSE) $ CH.RawQuery $ T.unpack queryRequest.queryText
  case eQueryResult of
    Right queryResult -> do
      checkForMissingFieldsInQueryResponse queryRequest.queryResults queryResult
      pure queryResult
    Left err -> throwError (InvalidRequest $ "Error while run clickhouse query: " <> T.pack err)

checkForMissingFieldsInQueryResponse :: (Log m, MonadThrow m) => [Text] -> [A.Object] -> m ()
checkForMissingFieldsInQueryResponse _ [] = pure ()
checkForMissingFieldsInQueryResponse queryResults (obj : _) = do
  let missingResults = flip mapMaybe queryResults $ \queryResult -> do
        case A.lookup (A.fromText queryResult) obj of
          Nothing -> Just queryResult
          Just _ -> Nothing
  case missingResults of
    [] -> pure ()
    _ : _ -> throwError (InvalidRequest $ "Missing fields in clickhouse response: " <> show missingResults)
