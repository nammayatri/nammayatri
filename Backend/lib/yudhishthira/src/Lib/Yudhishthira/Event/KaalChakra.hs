module Lib.Yudhishthira.Event.KaalChakra
  ( Handle (..),
    kaalChakraEvent,
    clearEventData,
    updateUserTagsHandler,
    Template.Template (..),
    runQueryRequestTemplate,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.Aeson.Types as A
import Data.Either.Extra (mapLeft)
import Data.List (nub)
import qualified Data.Map as M
import Data.Scientific (fromFloatDigits, toRealFloat)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import qualified Data.Vector as V
import qualified JsonLogic
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Event.KaalChakra.Parse as Parse
import qualified Lib.Yudhishthira.Event.KaalChakra.Template as Template
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import Lib.Yudhishthira.Storage.Queries.ChakraQueries as QChakraQueries
import qualified Lib.Yudhishthira.Storage.Queries.NammaTag as QNammaTag
import qualified Lib.Yudhishthira.Storage.Queries.UserData as QUserData
import qualified Lib.Yudhishthira.Storage.Queries.UserDataExtra as QUserDataE
import Lib.Yudhishthira.Tools.Error
import Lib.Yudhishthira.Tools.Utils
import qualified Lib.Yudhishthira.Types as Yudhishthira
import qualified Lib.Yudhishthira.Types.ChakraQueries
import qualified Lib.Yudhishthira.Types.NammaTag as DNT
import qualified Lib.Yudhishthira.Types.UserData as DUserData

data Handle m action = Handle
  { getUserTags :: Id Yudhishthira.User -> m (Maybe [Yudhishthira.TagNameValue]), -- Nothing if user not found
    updateUserTags :: Id Yudhishthira.User -> [Yudhishthira.TagNameValue] -> m (),
    action :: Id Yudhishthira.User -> action -> m ()
  }

--  which is log level in PROD?
skipUpdateUserTagsHandler :: forall m action. (Monad m, Log m, MonadThrow m, Read action, Show action) => Handle m action
skipUpdateUserTagsHandler =
  Handle
    { getUserTags = \userId -> logInfo ("Skip update user tags in DB selected: userId: " <> show userId) >> pure (Just []),
      updateUserTags = \userId updatedTags -> logInfo $ "Skip update user tags in DB selected: userId: " <> show userId <> "; updated tags: " <> show updatedTags,
      action = \userId action -> logInfo $ "Skip action: " <> show action <> "; userId: " <> show userId
    }

kaalChakraEvent ::
  forall m r.
  (BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) =>
  Yudhishthira.RunKaalChakraJobReq ->
  m Yudhishthira.RunKaalChakraJobRes
kaalChakraEvent req = do
  eventId <- getEventId req.chakra
  handle (errHandler eventId) (kaalChakraEventInternal eventId req)
  where
    errHandler eventId err = case req.action of
      Yudhishthira.RUN -> throwM @m @SomeException err
      Yudhishthira.SCHEDULE _ -> do
        logError $ "Fetch user data job failed: " <> show err
        pure Yudhishthira.RunKaalChakraJobRes {eventId = Just eventId, tags = Nothing, users = Nothing, chakraBatchState = Yudhishthira.Failed}

kaalChakraEventInternal ::
  (BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) =>
  Id Yudhishthira.Event ->
  Yudhishthira.RunKaalChakraJobReq ->
  m Yudhishthira.RunKaalChakraJobRes
kaalChakraEventInternal eventId req = withLogTag ("EventId-" <> eventId.getId) do
  startTime <- getCurrentTime
  logInfo $
    "Running kaal-chakra event: chakra: "
      <> show req.chakra
      <> "; update user tags: "
      <> show req.updateUserTags
      <> "; parse query results: "
      <> show req.parseQueryResults
      <> "; users set: "
      <> show req.usersSet
      <> "; users in batch: "
      <> show req.usersInBatch
      <> "; max batches: "
      <> show req.maxBatches
  chakraQueries <- QChakraQueries.findAllByChakra req.chakra
  bn <- nextChakraBatchNumber req.chakra
  let batchesNumber = bn - 1
  batchedUserData <- fetchUserDataBatch req eventId chakraQueries batchesNumber
  let usersNumber = length batchedUserData.userIds
  endTime <- getCurrentTime
  let durationInSeconds = Time.nominalDiffTimeToSeconds $ diffUTCTime endTime startTime
  logInfo $ "User data fetched successfully: batches number: " <> show batchesNumber <> "; users number: " <> show usersNumber
  logDebug $ "took time: " <> show durationInSeconds
  pure $ Yudhishthira.RunKaalChakraJobRes (Just eventId) Nothing Nothing batchedUserData.chakraBatchState

updateUserTagsHandler ::
  forall m r action.
  (BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, Read action, Show action) =>
  Handle m action ->
  Yudhishthira.UpdateKaalBasedTagsJobReq ->
  m Yudhishthira.RunKaalChakraJobRes
updateUserTagsHandler h req = do
  handle (errHandler req.eventId) $
    if req.updateUserTags
      then updateUserTagsHandlerInternal h req
      else updateUserTagsHandlerInternal (skipUpdateUserTagsHandler @m @action) req
  where
    errHandler eventId (err :: SomeException) = do
      logError $ "Update user tags job failed: " <> show err
      pure Yudhishthira.RunKaalChakraJobRes {eventId = Just eventId, tags = Nothing, users = Nothing, chakraBatchState = Yudhishthira.Failed}

updateUserTagsHandlerInternal ::
  (BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, Read action, Show action) =>
  Handle m action ->
  Yudhishthira.UpdateKaalBasedTagsJobReq ->
  m Yudhishthira.RunKaalChakraJobRes
updateUserTagsHandlerInternal h req = withLogTag ("EventId-" <> req.eventId.getId) do
  let eventId = req.eventId
  startTime <- getCurrentTime
  chakraQueries <- QChakraQueries.findAllByChakra req.chakra
  tags <- QNammaTag.findAllByChakra req.chakra

  -- Skip LLM tags instead of throwing error
  filteredTags <- flip filterM tags $ \tag -> case tag.rule of
    Yudhishthira.RuleEngine _ -> pure True
    Yudhishthira.LLM _ -> do
      logError $ "LLM is not implemented: tag: " <> tag.name <> "; skipping."
      pure False

  bn <- nextChakraBatchNumber req.chakra
  let batchNumber = bn - 1
  let limit = Just $ req.usersInBatch
  let offset = Just $ batchNumber * req.usersInBatch
  batchedUserData <- QUserDataE.findAllByEventIdWithLimitOffset eventId limit offset
  logDebug $ "Running update user tags batch: " <> show batchNumber <> "; users found: " <> show (length batchedUserData)
  -- getting this, but maybe limit offset will leave some part of the data of trailing guyz begind so using this just as a medium to get the userIds to do tagging for.

  let defaultUserDataMap = Parse.mkDefaultUserDataMap chakraQueries
  res <- case req.usersSet of
    Yudhishthira.ALL_USERS -> do
      if null batchedUserData
        then pure $ Yudhishthira.RunKaalChakraJobRes {eventId = Just eventId, tags = Nothing, users = Nothing, chakraBatchState = Yudhishthira.Completed}
        else do
          if batchNumber > req.maxBatches
            then do
              void $ decrChakraBatchNumber req.chakra
              logError $ "Reached max batch size: " <> show batchNumber <> "; for updateTags job."
              pure $ Yudhishthira.RunKaalChakraJobRes {eventId = Just eventId, tags = Nothing, users = Nothing, chakraBatchState = Yudhishthira.Completed}
            else do
              forM_ batchedUserData $
                kaalChakraEventUser h filteredTags eventId defaultUserDataMap . (.userId)
              pure $ Yudhishthira.RunKaalChakraJobRes {eventId = Just eventId, tags = Nothing, users = Nothing, chakraBatchState = Yudhishthira.Continue req.batchDelayInSec}
    _ -> do
      usersAPIEntity <- forM batchedUserData $ \(DUserData.UserData {userId}) -> do
        kaalChakraEventUser h filteredTags eventId defaultUserDataMap userId
      let tagsAPIEntity = mkTagAPIEntity <$> filteredTags
      pure $ Yudhishthira.RunKaalChakraJobRes {eventId = Just eventId, tags = Just tagsAPIEntity, users = Just usersAPIEntity, chakraBatchState = Yudhishthira.Completed}
  endTime <- getCurrentTime
  let durationInSeconds = Time.nominalDiffTimeToSeconds $ diffUTCTime endTime startTime
  logInfo $ "User tags updated successfully in " <> show durationInSeconds <> " seconds; batches number: " <> show batchNumber
  pure res

data ChakraBatchedUserData = ChakraBatchedUserData
  { userIds :: S.Set (Id Yudhishthira.User),
    chakraBatchState :: Yudhishthira.ChakraBatchState
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

fetchUserDataBatch ::
  (BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) =>
  Yudhishthira.RunKaalChakraJobReq ->
  Id Yudhishthira.Event ->
  [Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries] ->
  Int ->
  m ChakraBatchedUserData
fetchUserDataBatch req eventId chakraQueries batchNumber = do
  logInfo $ "Running batch: " <> show batchNumber
  when (req.usersInBatch < 1) $ throwError (InvalidRequest "Quantity of users in batch should be more than 0")
  let limit = Yudhishthira.QLimit req.usersInBatch
  let offset = Yudhishthira.QOffset $ batchNumber * req.usersInBatch
  let template = Template.Template {limit, offset, usersSet = req.usersSet}
  chakraQueriesResults <- forM chakraQueries $ \chakraQuery -> do
    queryResults <- runQueryRequestTemplate chakraQuery template
    if req.parseQueryResults
      then do
        let notParsedObjects = queryResults.queryResultObjects
        parsedObjects <- Parse.parseQueryResult chakraQuery notParsedObjects
        pure $ queryResults{queryResultObjects = parsedObjects}
      else pure queryResults

  let notEmptyResults = filter (\res -> not (null res.queryResultObjects)) chakraQueriesResults
  case notEmptyResults of
    [] -> do
      logError $ "Got empty result for the batchNumber:" <> show batchNumber <> ", chakra successfully finished, rescheduling for the next chakra"
      pure $ ChakraBatchedUserData S.empty Yudhishthira.Completed
    _ -> do
      if batchNumber > req.maxBatches
        then do
          logError $ "Exceeded number of batched, reached:" <> show batchNumber <> ", finishing job"
          void $ decrChakraBatchNumber req.chakra
          pure $ ChakraBatchedUserData S.empty Yudhishthira.Completed
        else do
          userDataList <- buildUserDataList req.chakra eventId batchNumber notEmptyResults
          QUserData.createMany userDataList
          let newUserIds = S.fromList $ userDataList <&> (.userId)
          pure $ ChakraBatchedUserData newUserIds (Yudhishthira.Continue req.batchDelayInSec)

kaalChakraEventUser ::
  forall m r action.
  (BeamFlow m r, Monad m, Log m, Read action, Show action) =>
  Handle m action ->
  [DNT.NammaTag] ->
  Id Yudhishthira.Event ->
  [Parse.DefaultDataMap] ->
  Id Yudhishthira.User ->
  m Yudhishthira.RunKaalChakraJobResForUser
kaalChakraEventUser h filteredTags eventId defaultUserDataMap userId = withLogTag ("UserId-" <> userId.getId) do
  userDataList <- QUserData.findAllByUserIdAndEventId userId eventId
  -- Skip current user instead of throwing error
  let eUserData = do
        userData <- foldlM appendUserDataValue (A.Object A.empty) $ userDataList <&> (.userDataValue)
        case userData of
          A.Object userDataObj -> pure . first A.Object $ Parse.appendDefaultValues userId userDataObj defaultUserDataMap
          _ -> Left $ "Object expected in user data: " <> show userData -- should never occur because appendUserDataValue always returns Object
  case eUserData of
    Right (userDataValue, accQueries) -> do
      unless (null accQueries) $ do
        logDebug $ "Used default objects for userId: " <> show userId <> "; query names: " <> show accQueries
      let eTagValuesTuple = applyRule userId userDataValue <$> filteredTags
      -- Skip current tag for current user instead of throwing error
      tagValuesTuple <- (catMaybes <$>) $
        forM eTagValuesTuple $ \case
          Right tagValueTuple -> pure $ Just tagValueTuple
          Left err -> logError err >> pure Nothing

      case tagValuesTuple of
        [] -> do
          logDebug ("No tags should be applied for current user: " <> show userId)
          pure $ mkRunKaalChakraJobResForUser userId userDataValue Nothing Nothing
        _ -> do
          mbOldTagsText <- h.getUserTags userId
          -- Skip current user instead of throwing error
          case updateUserTagValues userId tagValuesTuple mbOldTagsText of
            Right (updTagsText, actionDataList) -> do
              if Just updTagsText /= mbOldTagsText
                then do
                  h.updateUserTags userId updTagsText
                  forM_ actionDataList $ \actionData -> do
                    runTagAction h userId actionData
                else logDebug $ "Tags did not changed for current user: " <> show userId
              pure $ mkRunKaalChakraJobResForUser userId userDataValue mbOldTagsText (Just updTagsText)
            Left err -> logError err $> mkRunKaalChakraJobResForUser userId userDataValue mbOldTagsText Nothing
    Left err -> logError err $> mkRunKaalChakraJobResForUser userId (A.Object A.empty) Nothing Nothing

runTagAction ::
  forall m action.
  (MonadFlow m, Read action, Show action) =>
  Handle m action ->
  Id Yudhishthira.User ->
  ActionData ->
  m ()
runTagAction h userId actionData = do
  let tagName = Yudhishthira.TagName actionData.tag.name
  let tagValueNew = actionData.tagValueNew
  if actionData.tagNameValueOld == Just actionData.tagNameValueNew
    then logDebug $ "Tag " <> show tagName <> " did not changed for user; skipping."
    else whenJust actionData.tag.actionEngine $ \tagActionEngine -> do
      case forM actionData.tagNameValueOld (parseTagValueFromText actionData.tag) of
        Left err -> do
          logError $ "Could not parse old tag value: tagName: " <> show tagName <> "; tagValue: " <> show actionData.tagNameValueOld <> "; error: " <> show err <> "; skipping."
        Right mbTagValueOld -> do
          let actionDataObject = mkActionDataObject tagValueNew mbTagValueOld
          let eTagActionObj = JsonLogic.jsonLogicEither tagActionEngine actionDataObject
          case eTagActionObj of
            Left err -> do
              logError $ "Could not apply extra tag action rule to data: tagName: " <> show tagName <> "; data: " <> show actionDataObject <> "; error: " <> show err <> "; skipping."
            Right (A.String actionTxt) -> case readMaybe @action (T.unpack actionTxt) of
              Just action -> do
                logInfo $ "Run extra tag action: tagName: " <> show tagName <> "; action: " <> show action <> "; tagValueOld: " <> show mbTagValueOld <> "; tagValueNew: " <> show tagValueNew
                handle (errHandler action) $
                  h.action userId action
              Nothing -> do
                logError $ "Could not parse action: tagName: " <> show tagName <> "; action: " <> actionTxt <> "; tagValueOld: " <> show mbTagValueOld <> "; tagValueNew: " <> show tagValueNew <> "; skipping."
            Right A.Null ->
              logInfo $ "Empty extra tag action determined: tagName:" <> show tagName <> "; tagValueOld: " <> show mbTagValueOld <> "; tagValueNew: " <> show tagValueNew
            Right val -> do
              -- only String or Null value supported for now, we can add Array later
              logError $ "String or Null expected for extra tag action: tagName: " <> show tagName <> "; data: " <> show val <> "; skipping."
  where
    errHandler action exc = logError $ "Extra tag action " <> show action <> " interrupted with error: " <> show @Text @SomeException exc

mkActionDataObject :: Yudhishthira.TagValue -> Maybe Yudhishthira.TagValue -> A.Value
mkActionDataObject tagValueNew mbTagValueOld = do
  A.object ["newValue" A..= convertTagValueToJSON tagValueNew, "oldValue" A..= (convertTagValueToJSON <$> mbTagValueOld)]

appendUserDataValue :: A.Value -> A.Value -> Either Text A.Value
appendUserDataValue (A.Object obj1) (A.Object obj2) | null (A.intersection obj1 obj2) = Right $ A.Object (A.union obj1 obj2)
appendUserDataValue (A.Object obj1) (A.Object obj2) = Left $ "User data for the same user should not intersect: object1: " <> show obj1 <> "; object2: " <> show obj2 <> "; skipping."
appendUserDataValue (A.Object _) val2 = Left $ "User data should be object: " <> show val2 <> "; skipping."
appendUserDataValue val1 _ = Left $ "User data should be object: " <> show val1 <> "; skipping."

mkTagAPIEntity :: DNT.NammaTag -> Yudhishthira.TagAPIEntity
mkTagAPIEntity DNT.NammaTag {..} = Yudhishthira.TagAPIEntity {..}

mkRunKaalChakraJobResForUser ::
  Id Yudhishthira.User ->
  A.Value ->
  Maybe [Yudhishthira.TagNameValue] ->
  Maybe [Yudhishthira.TagNameValue] ->
  Yudhishthira.RunKaalChakraJobResForUser
mkRunKaalChakraJobResForUser userId userDataValue userOldTags userUpdatedTags = do
  Yudhishthira.RunKaalChakraJobResForUser {..}

applyRule ::
  Id Yudhishthira.User ->
  A.Value ->
  DNT.NammaTag ->
  Either Text (DNT.NammaTag, Yudhishthira.TagValue)
applyRule userId userDataValue tag = case tag.rule of
  Yudhishthira.RuleEngine logic -> do
    let eTagValueObj = JsonLogic.jsonLogicEither logic userDataValue
    case eTagValueObj of
      Left err -> do
        Left $ "Could not apply rule to data: userId: " <> show userId <> "; tag: " <> show tag.name <> "; data: " <> show userDataValue <> "; error: " <> show err <> "; skipping."
      Right tagValueObj -> do
        case parseTagValue tagValueObj tag.possibleValues of
          Nothing -> Left $ "Value is not allowed: " <> show tagValueObj <> "; userId: " <> show userId <> "; tag: " <> tag.name <> "; skipping."
          Just tagValue -> Right (tag, tagValue)
  Yudhishthira.LLM _ -> Left $ "LLM is not implemented: tag: " <> tag.name <> "; skipping."

type TagsMap = M.Map Yudhishthira.TagName

updateUserTagValues ::
  Id Yudhishthira.User ->
  [(DNT.NammaTag, Yudhishthira.TagValue)] ->
  Maybe [Yudhishthira.TagNameValue] ->
  Either Text ([Yudhishthira.TagNameValue], [ActionData])
updateUserTagValues userId _ Nothing = Left $ "User with userId: " <> show userId <> " did not found; skipping."
updateUserTagValues userId updatedTags (Just oldTagsText) = do
  oldTagsMap <- (M.fromList <$>) $
    forM oldTagsText $ \oldTagText -> do
      case parseTagName oldTagText of
        Nothing -> Left $ "Could not parse tag name: " <> show oldTagText <> "; userId: " <> show userId <> "; skipping."
        Just oldTagName -> Right (oldTagName, oldTagText)
  let (updTagsMap, actionDataMap) = foldl foldFunc (oldTagsMap, M.empty :: TagsMap ActionData) updatedTags
  Right (snd <$> M.toList updTagsMap, snd <$> M.toList actionDataMap)
  where
    foldFunc ::
      (TagsMap Yudhishthira.TagNameValue, TagsMap ActionData) ->
      (DNT.NammaTag, Yudhishthira.TagValue) ->
      (TagsMap Yudhishthira.TagNameValue, TagsMap ActionData)
    foldFunc (tagNameValueMapOld, actionDataMapOld) (tag, tagValueNew) = do
      let tagName = Yudhishthira.TagName tag.name
          tagNameValueOld = M.lookup tagName tagNameValueMapOld
          tagNameValueNew = showTag tagName tagValueNew
          tagNameValueMapNew = M.insert tagName tagNameValueNew tagNameValueMapOld
          actionData = ActionData {tag, tagValueNew, tagNameValueNew, tagNameValueOld}
          actionDataMapNew = M.insert tagName actionData actionDataMapOld
      (tagNameValueMapNew, actionDataMapNew)

data ActionData = ActionData
  { tag :: DNT.NammaTag,
    tagValueNew :: Yudhishthira.TagValue,
    tagNameValueNew :: Yudhishthira.TagNameValue,
    tagNameValueOld :: Maybe Yudhishthira.TagNameValue -- only if it was present earlier
  }
  deriving (Show)

parseTagName :: Yudhishthira.TagNameValue -> Maybe Yudhishthira.TagName
parseTagName (Yudhishthira.TagNameValue txt) = case T.splitOn "#" txt of
  (tagName : _) -> Just (Yudhishthira.TagName tagName)
  _ -> Nothing

showTag ::
  Yudhishthira.TagName ->
  Yudhishthira.TagValue ->
  Yudhishthira.TagNameValue
showTag (Yudhishthira.TagName tagName) (Yudhishthira.TextValue tagValueText) = Yudhishthira.TagNameValue $ tagName <> "#" <> tagValueText
showTag (Yudhishthira.TagName tagName) (Yudhishthira.NumberValue tagValueDouble) = Yudhishthira.TagNameValue $ tagName <> "#" <> show tagValueDouble
showTag (Yudhishthira.TagName tagName) (Yudhishthira.ArrayValue tagValueArray) = Yudhishthira.TagNameValue $ tagName <> "#" <> T.intercalate "&" tagValueArray

-- inverse conversion for showTag
parseTagValueFromText :: DNT.NammaTag -> Yudhishthira.TagNameValue -> Either Text Yudhishthira.TagValue
parseTagValueFromText tag (Yudhishthira.TagNameValue txt) = case T.splitOn "#" txt of
  [_, tagValue] -> do
    case tag.possibleValues of
      Yudhishthira.Range _d1 _d2 -> do
        tagDouble <- mapLeft ("Couldn't parse double value: " <>) $ readEither @Text @Double tagValue
        pure $ Yudhishthira.NumberValue tagDouble
      _ -> case T.splitOn "&" tagValue of
        [tagValueText] -> pure $ Yudhishthira.TextValue tagValueText -- we can't separate text value from array containting one item
        [] -> Left "Tag value should not be empty"
        tagValueArray -> pure $ Yudhishthira.ArrayValue tagValueArray
  _ -> Left "Tag should have format tagName#tagValue"

parseTagValue :: A.Value -> Yudhishthira.TagValues -> Maybe Yudhishthira.TagValue
parseTagValue (A.String txt) possibleValues = case possibleValues of
  Yudhishthira.Tags possibleTxt | txt `elem` possibleTxt -> Just $ Yudhishthira.TextValue txt
  Yudhishthira.Tags _ -> Nothing
  Yudhishthira.AnyText -> Just $ Yudhishthira.TextValue txt
  Yudhishthira.Range minDouble maxDouble -> case readMaybe @Double (T.unpack txt) of
    Just d | d >= minDouble && d <= maxDouble -> Just $ Yudhishthira.NumberValue d
    _ -> Nothing
parseTagValue (A.Number sci) possibleValues = do
  let d = toRealFloat sci
  case possibleValues of
    Yudhishthira.Range minDouble maxDouble | d >= minDouble && d <= maxDouble -> Just $ Yudhishthira.NumberValue d
    _ -> Nothing
parseTagValue (A.Array arr') _ = Yudhishthira.ArrayValue <$> (mapM extractText (toList arr')) -- Add possible value checks
parseTagValue _ _ = Nothing

-- inverse conversion for parseTagValue
convertTagValueToJSON :: Yudhishthira.TagValue -> A.Value
convertTagValueToJSON (Yudhishthira.TextValue txt) = A.String txt
convertTagValueToJSON (Yudhishthira.NumberValue d) = A.Number $ fromFloatDigits @Double d
convertTagValueToJSON (Yudhishthira.ArrayValue arr') = A.Array $ V.fromList $ A.String <$> arr'

extractText :: A.Value -> Maybe Text
extractText (A.String txt) = Just txt
extractText _ = Nothing

buildUserDataList ::
  (MonadFlow m) =>
  Yudhishthira.Chakra ->
  Id Yudhishthira.Event ->
  Int ->
  [ChakraQueryResult] ->
  m [DUserData.UserData]
buildUserDataList chakra eventId batchNumber chakraQueriesResults = do
  queryResultsMapping :: [(Id Yudhishthira.User, Text, A.Object)] <-
    (concat <$>) $
      forM chakraQueriesResults $ \ChakraQueryResult {queryName, queryResultObjects} -> do
        forM queryResultObjects $ \chakraQueryResultForUser -> do
          userId <- parseUserId chakraQueryResultForUser
          pure (userId, queryName, chakraQueryResultForUser)
  let userIds = nub $ map (\(userId, _, _) -> userId) queryResultsMapping

  forM userIds $ \userId -> do
    let queryResultsForUser = filter (\(userId', _, _) -> userId' == userId) queryResultsMapping
    let userDataValue = A.Object $ A.fromList $ queryResultsForUser <&> \(_userId, queryName, obj) -> A.fromText queryName A..= obj
    id <- generateGUID
    now <- getCurrentTime
    pure $ DUserData.UserData {id, chakra, eventId, batchNumber, userDataValue, userId, createdAt = now, updatedAt = now}

parseUserId :: (MonadThrow m, Log m) => A.Object -> m (Id Yudhishthira.User)
parseUserId obj = do
  let mbUserId = flip A.parseMaybe obj $ \obj' ->
        obj' A..: A.fromText userIdField
  mbUserId & fromMaybeM (InternalError $ "userId not found in chakra query response: " <> show obj)

data ChakraQueryResult = ChakraQueryResult
  { queryName :: Text,
    queryText :: Text,
    queryResultObjects :: [A.Object]
  }

runQueryRequestTemplate ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries ->
  Template.Template ->
  m ChakraQueryResult
runQueryRequestTemplate chakraQuery template = do
  case Template.replaceTemplateUnits (Template.RawQuery chakraQuery.queryText) template of
    Right (Template.RawQuery rawQuery) -> do
      res <- runQueryRequest chakraQuery{queryText = rawQuery}
      pure
        ChakraQueryResult
          { queryName = chakraQuery.queryName,
            queryText = rawQuery,
            queryResultObjects = res
          }
    Left err -> throwError $ InvalidRequest err

runQueryRequest ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries ->
  m [A.Object]
runQueryRequest queryRequest = do
  logDebug $ "Run query request: name:" <> show queryRequest.queryName <> "; raw query: '" <> queryRequest.queryText <> "'"
  eQueryResult :: Either String [A.Object] <- CH.runRawQuery (Proxy @CH.APP_SERVICE_CLICKHOUSE) $ CH.RawQuery $ T.unpack queryRequest.queryText
  case eQueryResult of
    Right queryResult -> do
      checkForMissingFieldsInQueryResponse (queryRequest.queryResults <&> (.resultName)) queryResult
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

clearEventData ::
  (BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) =>
  Yudhishthira.Chakra ->
  Maybe (Id Yudhishthira.Event) ->
  m ()
clearEventData chakra (Just eventId) = do
  logInfo $ "finished update tag job for event " <> show eventId
  resetChakraBatchNumber chakra
  logInfo $ "batch number reset now"
  delChakraEventId chakra
  QUserDataE.deleteUserDataWithEventId eventId
  logInfo $ "deleted all event user data"
clearEventData chakra Nothing = do
  logInfo $ "finished get user data job"
  resetChakraBatchNumber chakra
  logInfo $ "batch number reset now"

-- keeping chakra event in redis
getEventId :: (BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Yudhishthira.Chakra -> m (Id Yudhishthira.Event)
getEventId chakra = do
  mbOngoingEventId <- Hedis.get $ mkChakraEventIdKey chakra
  case mbOngoingEventId of
    Just ongoingEventId -> pure $ Id ongoingEventId
    Nothing -> do
      eventId <- Id <$> generateGUID
      cacheChakraEventId chakra eventId
      pure eventId

cacheChakraEventId :: (BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Yudhishthira.Chakra -> Id Yudhishthira.Event -> m ()
cacheChakraEventId chakra eventId = Hedis.setExp (mkChakraEventIdKey chakra) eventId.getId 64800

mkChakraEventIdKey :: Yudhishthira.Chakra -> Text
mkChakraEventIdKey chakra = "chakra_event_id:" <> show chakra

delChakraEventId :: (BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Yudhishthira.Chakra -> m ()
delChakraEventId chakra = Hedis.del (mkChakraEventIdKey chakra)

-- resetting chakra batchNumber for next days chakra as all users are exhausted
batchNumberKey :: Yudhishthira.Chakra -> Text
batchNumberKey chakra = "kaal_chkra_batch_number:" <> show chakra

resetChakraBatchNumber :: (CacheFlow m r, Monad m, Log m, MonadFlow m) => Yudhishthira.Chakra -> m ()
resetChakraBatchNumber = Hedis.del . batchNumberKey

decrChakraBatchNumber :: (CacheFlow m r, Monad m, Log m, MonadFlow m) => Yudhishthira.Chakra -> m Int
decrChakraBatchNumber = fmap fromIntegral . (\a -> Hedis.decr a <* Hedis.expire a 64800) . batchNumberKey

nextChakraBatchNumber :: (CacheFlow m r, Monad m, Log m, MonadFlow m) => Yudhishthira.Chakra -> m Int
nextChakraBatchNumber = fmap fromIntegral . (\a -> Hedis.incr a <* Hedis.expire a 64800) . batchNumberKey
