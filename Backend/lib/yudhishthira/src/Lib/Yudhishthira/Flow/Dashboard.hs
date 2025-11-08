module Lib.Yudhishthira.Flow.Dashboard where

import qualified ConfigPilotFrontend.Common as CPFC
import qualified ConfigPilotFrontend.Flow as CPF
import qualified ConfigPilotFrontend.Types as CPT
import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Aeson as A
import Data.List (groupBy, nub, sortBy)
import qualified Data.List.NonEmpty as DLNE
import qualified Data.List.NonEmpty as NE
import Data.Scientific (toRealFloat)
import qualified Data.Text as T
import JsonLogic
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.TimeBound
import Kernel.Utils.Common
import Lib.Yudhishthira.Event.KaalChakra as KaalChakraEvent
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicElement as CADLE
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout as CADLR
import qualified Lib.Yudhishthira.Storage.CachedQueries.TimeBoundConfig as CQTBC
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicElement as QADLE
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicRollout as LYSQADLR
import qualified Lib.Yudhishthira.Storage.Queries.ChakraQueries as QChakraQueries
import qualified Lib.Yudhishthira.Storage.Queries.ChakraQueries as SQCQ
import qualified Lib.Yudhishthira.Storage.Queries.NammaTag as QNT
import qualified Lib.Yudhishthira.Storage.Queries.NammaTagTrigger as QNTT
import Lib.Yudhishthira.Tools.Error
import Lib.Yudhishthira.Tools.Utils
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types.AppDynamicLogicElement as DTADLE
import Lib.Yudhishthira.Types.AppDynamicLogicRollout
import qualified Lib.Yudhishthira.Types.ChakraQueries
import qualified Lib.Yudhishthira.Types.ChakraQueries as LYTCQ
import qualified Lib.Yudhishthira.Types.NammaTag as DNT
import qualified Lib.Yudhishthira.Types.NammaTagTrigger as DNTT
import Lib.Yudhishthira.Types.TimeBoundConfig
import qualified System.Environment as Se

postTagCreate :: forall m r. BeamFlow m r => Lib.Yudhishthira.Types.CreateNammaTagRequest -> m Kernel.Types.APISuccess.APISuccess
postTagCreate tagRequest = do
  now <- getCurrentTime
  let nammaTag = mkNammaTag now
  mbNammaTagTriggers <- buildNammaTagTriggers now
  checkForDuplicacy nammaTag.name

  QNT.create nammaTag
  QNTT.deleteAllByTagName nammaTag.name -- Just in case if old data persists
  whenJust mbNammaTagTriggers (QNTT.createMany . NE.toList)
  pure Kernel.Types.APISuccess.Success
  where
    checkForDuplicacy name = do
      QNT.findByPrimaryKey name >>= \case
        Just _ -> throwError (TagAlreadyExists name)
        Nothing -> pure ()

    mkNammaTag now = do
      let createdAt = now
          updatedAt = now
      case tagRequest of
        Lib.Yudhishthira.Types.ApplicationTag Lib.Yudhishthira.Types.NammaTagApplication {..} ->
          DNT.NammaTag
            { category = tagCategory,
              info = DNT.Application,
              name = tagName,
              possibleValues = tagPossibleValues,
              rule = tagRule,
              description = description,
              actionEngine = Nothing,
              validity = tagValidity,
              ..
            }
        Lib.Yudhishthira.Types.KaalChakraTag Lib.Yudhishthira.Types.NammaTagChakra {..} ->
          DNT.NammaTag
            { category = tagCategory,
              info = DNT.KaalChakra (DNT.KaalChakraTagInfo tagChakra),
              name = tagName,
              possibleValues = tagPossibleValues,
              rule = tagRule,
              description = description,
              actionEngine,
              validity = tagValidity,
              ..
            }
        Lib.Yudhishthira.Types.ManualTag Lib.Yudhishthira.Types.NammaTagManual {..} ->
          DNT.NammaTag
            { category = tagCategory,
              info = DNT.Manual,
              name = tagName,
              possibleValues = tagPossibleValues,
              rule = Lib.Yudhishthira.Types.LLM "empty-context",
              description = description,
              actionEngine = Nothing,
              validity = tagValidity,
              ..
            }

    buildNammaTagTriggers :: UTCTime -> m (Maybe (NE.NonEmpty DNTT.NammaTagTrigger))
    buildNammaTagTriggers now = case tagRequest of
      Lib.Yudhishthira.Types.ApplicationTag Lib.Yudhishthira.Types.NammaTagApplication {..} -> do
        let createdAt = now
            updatedAt = now
        unless (length tagStages == length (NE.nub tagStages)) $
          throwError (InvalidRequest "Tag stages should be unique")
        pure . Just $ tagStages <&> \event -> DNTT.NammaTagTrigger {event, tagName, ..}
      _ -> pure Nothing

postTagUpdate :: forall m r. BeamFlow m r => Lib.Yudhishthira.Types.UpdateNammaTagRequest -> m Kernel.Types.APISuccess.APISuccess
postTagUpdate tagRequest = do
  tag <- QNT.findByPrimaryKey tagRequest.tagName >>= fromMaybeM (InvalidRequest "Tag not found in the system, please create the tag")
  now <- getCurrentTime

  let updatedTag = mkUpdateNammaTagEntity tag now
  mbNammaTagTriggers <- buildNammaTagTriggers tag now

  QNT.updateByPrimaryKey updatedTag
  whenJust mbNammaTagTriggers \nammaTagTriggers -> do
    QNTT.deleteAllByTagName tagRequest.tagName
    QNTT.createMany (NE.toList nammaTagTriggers)
  return Kernel.Types.APISuccess.Success
  where
    mkUpdateNammaTagEntity tag now = do
      let validity = case tagRequest.resetTagValidity of
            Just True -> Nothing
            _ -> tagRequest.tagValidity <|> tag.validity
      DNT.NammaTag
        { category = fromMaybe tag.category tagRequest.tagCategory,
          info = mkTagInfo tag,
          name = tag.name,
          possibleValues = fromMaybe tag.possibleValues tagRequest.tagPossibleValues,
          rule = fromMaybe tag.rule tagRequest.tagRule,
          description = tagRequest.description <|> tag.description,
          actionEngine = tagRequest.actionEngine <|> tag.actionEngine,
          validity,
          createdAt = tag.createdAt,
          updatedAt = now
        }
    mkTagInfo tag = case tag.info of
      DNT.Application -> DNT.Application
      DNT.KaalChakra (DNT.KaalChakraTagInfo tagChakra) -> DNT.KaalChakra (DNT.KaalChakraTagInfo (fromMaybe tagChakra tagRequest.tagChakra))
      DNT.Manual -> DNT.Manual

    buildNammaTagTriggers :: DNT.NammaTag -> UTCTime -> m (Maybe (NE.NonEmpty DNTT.NammaTagTrigger))
    buildNammaTagTriggers tag now = do
      case tag.info of
        DNT.Application -> do
          let createdAt = now
              updatedAt = now
          whenJust tagRequest.tagStages $ \events -> do
            unless (length events == length (NE.nub events)) $
              throwError (InvalidRequest "Tag stages should be unique")
          pure $ tagRequest.tagStages <&> fmap \event -> DNTT.NammaTagTrigger {event, tagName = tagRequest.tagName, ..}
        _ -> do
          whenJust tagRequest.tagStages $ \_ -> throwError (InvalidRequest "tagStage relevant only for application tags")
          pure Nothing

deleteTag :: BeamFlow m r => T.Text -> m Kernel.Types.APISuccess.APISuccess
deleteTag tagName = do
  QNTT.deleteAllByTagName tagName
  QNT.deleteByPrimaryKey tagName
  return Kernel.Types.APISuccess.Success

postQueryCreate ::
  (BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) =>
  Lib.Yudhishthira.Types.ChakraQueriesAPIEntity ->
  m Kernel.Types.APISuccess.APISuccess
postQueryCreate queryRequest = do
  validateQueryResults queryRequest.queryResults
  existingChakraQuery <- SQCQ.findByPrimaryKey queryRequest.chakra queryRequest.queryName
  whenJust existingChakraQuery $ \_ -> do
    throwError $ ChakraQueriesAlreadyExists (show queryRequest.chakra) queryRequest.queryName
  chakraQuery <- buildQuery

  -- TODO find a better way to validate query than run it
  void $ KaalChakraEvent.runQueryRequestTemplate chakraQuery defaultTemplate
  SQCQ.create chakraQuery

  pure Kernel.Types.APISuccess.Success
  where
    buildQuery = do
      now <- getCurrentTime
      let Lib.Yudhishthira.Types.ChakraQueriesAPIEntity {..} = queryRequest
      return $ Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries {createdAt = now, updatedAt = now, ..}

postQueryUpdate ::
  (BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) =>
  Lib.Yudhishthira.Types.ChakraQueryUpdateReq ->
  m Kernel.Types.APISuccess.APISuccess
postQueryUpdate queryRequest = do
  whenJust queryRequest.queryResults validateQueryResults
  existingChakraQuery <-
    SQCQ.findByPrimaryKey queryRequest.chakra queryRequest.queryName >>= fromMaybeM (ChakraQueriesDoesNotExist (show queryRequest.chakra) queryRequest.queryName)
  chakraQuery <- buildUpdatedQuery existingChakraQuery

  void $ KaalChakraEvent.runQueryRequestTemplate chakraQuery defaultTemplate
  SQCQ.updateByPrimaryKey chakraQuery

  pure Kernel.Types.APISuccess.Success
  where
    buildUpdatedQuery existingChakraQuery = do
      let queryResults = fromMaybe existingChakraQuery.queryResults queryRequest.queryResults
      let queryText = fromMaybe existingChakraQuery.queryText queryRequest.queryText
      return $ existingChakraQuery{queryResults = queryResults, queryText = queryText}

validateQueryResults :: (MonadThrow m, Log m) => [Lib.Yudhishthira.Types.QueryResult] -> m ()
validateQueryResults newQueryFields = do
  checkIfMandatoryFieldsArePresent
  checkIfFieldsAreNotRepeated $ newQueryFields <&> (.resultName)
  where
    checkIfMandatoryFieldsArePresent = do
      let missingFields = filter (\field -> field `notElem` (newQueryFields <&> (.resultName))) mandatoryChakraFields
      unless (null missingFields) $ throwError (MissingQueryFields missingFields)

    checkIfFieldsAreNotRepeated newQueryFieldNames = do
      let repeatedFields = filter (\f -> length (filter (== f) newQueryFieldNames) > 1) newQueryFieldNames
      unless (null repeatedFields) $ throwError (RepeatedQueryFields repeatedFields)

defaultTemplate :: KaalChakraEvent.Template
defaultTemplate =
  KaalChakraEvent.Template
    { limit = Lib.Yudhishthira.Types.QLimit 10,
      offset = Lib.Yudhishthira.Types.QOffset 0,
      usersSet = Lib.Yudhishthira.Types.ALL_USERS
    }

queryDelete ::
  BeamFlow m r =>
  Lib.Yudhishthira.Types.ChakraQueryDeleteReq ->
  m Kernel.Types.APISuccess.APISuccess
queryDelete queryRequest = do
  void $ SQCQ.findByPrimaryKey queryRequest.chakra queryRequest.queryName >>= fromMaybeM (ChakraQueriesDoesNotExist (show queryRequest.chakra) queryRequest.queryName)
  SQCQ.deleteByPrimaryKey queryRequest.chakra queryRequest.queryName
  return Kernel.Types.APISuccess.Success

verifyTag :: BeamFlow m r => Lib.Yudhishthira.Types.TagNameValue -> m (Maybe DNT.NammaTag)
verifyTag (Lib.Yudhishthira.Types.TagNameValue fullTag) = do
  case T.splitOn "#" fullTag of
    [name, tagValueText] -> do
      tag <- QNT.findByPrimaryKey name >>= fromMaybeM (InvalidRequest "Tag not found in the system, please create the tag")
      if ("&" `T.isInfixOf` tagValueText) -- don't check for condition if value type is array
        then pure ()
        else do
          -- Normalize boolean values before processing - force them to be strings
          let normalizedTagValueText = case T.toLower tagValueText of
                "true" -> "\"true\""
                "false" -> "\"false\""
                _ -> tagValueText
          let mbTagValue = textToMaybeValue normalizedTagValueText
          case tag.possibleValues of
            Lib.Yudhishthira.Types.Tags values ->
              case mbTagValue of
                Just (A.String str) -> unless (str `elem` values) $ throwError (InvalidRequest $ "Tag value should be one of " <> show values)
                _ -> throwError $ InvalidRequest "Tag value should be a string"
            Lib.Yudhishthira.Types.Range start end ->
              case mbTagValue of
                Just (A.Number num) -> unless (num >= realToFrac start && num <= realToFrac end) $ throwError (InvalidRequest $ "Tag value should be between " <> show start <> " and " <> show end)
                _ -> throwError $ InvalidRequest "Tag value should be a number"
            Lib.Yudhishthira.Types.AnyText -> pure ()
      return (Just tag)
    [name] -> QNT.findByPrimaryKey name -- this is required for update of tag expiry
    _ -> throwError $ InvalidRequest "Tag should have format of name#value or just name"

postRunKaalChakraJob ::
  (BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m, Show action, Read action) =>
  Handle m action ->
  Lib.Yudhishthira.Types.RunKaalChakraJobReq ->
  m Lib.Yudhishthira.Types.RunKaalChakraJobRes
postRunKaalChakraJob _h req =
  case req.usersSet of
    Lib.Yudhishthira.Types.ALL_USERS -> throwError (InvalidRequest "ALL_USERS option available only from kaal-chakra scheduler")
    _ -> kaalChakraEvent req

createLogicData :: (FromJSON a, ToJSON a, BeamFlow m r) => a -> Maybe A.Value -> m a
createLogicData defaultVal Nothing = return defaultVal
createLogicData defaultVal (Just inputValue) = do
  let defaultValue = A.toJSON defaultVal
      finalValue = deepMerge defaultValue inputValue
  case A.fromJSON finalValue of
    A.Success a -> return a
    A.Error err -> throwError $ InvalidRequest ("Not able to merge input data into default value. Getting error: " <> show err)

verifyDynamicLogic :: (BeamFlow m r, ToJSON a) => Lib.Yudhishthira.Types.TagValues -> [Value] -> a -> m Lib.Yudhishthira.Types.RunLogicResp
verifyDynamicLogic tagPossibleValues logics data_ = do
  result <- runLogics logics data_
  let validResult =
        case tagPossibleValues of
          Lib.Yudhishthira.Types.Tags possibletags -> result.result `elem` map A.toJSON possibletags
          Lib.Yudhishthira.Types.AnyText -> isString result.result
          Lib.Yudhishthira.Types.Range start end -> inRange result.result (start, end)
  if validResult
    then pure result
    else throwError $ InvalidRequest $ "Returned result is not in possible tag values, got -> " <> show result

data VerifyTagData = VerifyTagData
  { possibleTags :: [Value],
    isAnyTextSupported :: Bool,
    possibleRanges :: [(Double, Double)]
  }

verifyEventLogic :: (BeamFlow m r, ToJSON a) => Lib.Yudhishthira.Types.ApplicationEvent -> [Value] -> a -> m Lib.Yudhishthira.Types.RunLogicResp
verifyEventLogic event logics data_ = do
  result <- runLogics logics data_
  nammaTagsTrigger <- QNTT.findAllByEvent event
  nammaTags <- QNT.findAllByPrimaryKeys (nammaTagsTrigger <&> (.tagName))
  let allTags =
        foldl'
          ( \(VerifyTagData tagsAcc isAnyText rangeAcc) x ->
              case x.possibleValues of
                Lib.Yudhishthira.Types.Tags pvs -> VerifyTagData (A.toJSON pvs : tagsAcc) isAnyText rangeAcc
                Lib.Yudhishthira.Types.AnyText -> VerifyTagData tagsAcc True rangeAcc
                Lib.Yudhishthira.Types.Range start end -> VerifyTagData tagsAcc isAnyText ((start, end) : rangeAcc)
          )
          (VerifyTagData [] False [])
          nammaTags
  if result.result `elem` allTags.possibleTags || isString result.result || any (inRange result.result) allTags.possibleRanges
    then return result
    else throwError $ InvalidRequest $ "Returned result is not possible tag values, got -> " <> show result

verifyAndUpdateDynamicLogic ::
  forall m r a b.
  (BeamFlow m r, ToJSON a, FromJSON b, Show a) =>
  Maybe (Id Lib.Yudhishthira.Types.Merchant) ->
  Proxy b ->
  Text ->
  Lib.Yudhishthira.Types.AppDynamicLogicReq ->
  a ->
  m Lib.Yudhishthira.Types.AppDynamicLogicResp
verifyAndUpdateDynamicLogic mbMerchantId _ referralLinkPassword req logicData = do
  resp <- runLogics req.rules logicData
  let shouldUpdateRule = fromMaybe False req.shouldUpdateRule
  let shouldVerifyOutput = fromMaybe False req.verifyOutput
  let errors = resp.errors <> (bool [] (verifyOutput resp.result) (shouldUpdateRule || shouldVerifyOutput))
  (isRuleUpdated, version) <-
    if shouldUpdateRule
      then do
        if null errors
          then do
            verifyPassword req.updatePassword -- Using referralLinkPassword as updatePassword, could be changed to a new field in future
            updateDynamicLogic req.rules req.domain
          else throwError $ InvalidRequest $ "Errors found in the rules" <> show errors
      else return (False, Nothing)
  return $ Lib.Yudhishthira.Types.AppDynamicLogicResp resp.result isRuleUpdated req.domain version errors
  where
    verifyPassword :: BeamFlow m r => Maybe Text -> m ()
    verifyPassword Nothing = throwError $ InvalidRequest "Password not provided"
    verifyPassword (Just updatePassword) =
      unless (updatePassword == referralLinkPassword) $ throwError $ InvalidRequest "Password does not match"

    updateDynamicLogic :: BeamFlow m r => [A.Value] -> Lib.Yudhishthira.Types.LogicDomain -> m (Bool, Maybe Int)
    updateDynamicLogic rules domain = do
      now <- getCurrentTime
      version <- do
        latestElement <- QADLE.findLatestVersion (Just 1) Nothing domain
        return (maybe 1 ((+ 1) . (.version)) (listToMaybe latestElement))
      let appDynamicLogics = zip rules [0 ..] <&> (\(rule, order) -> mkAppDynamicLogicElement version rule order now)
      CADLE.createMany appDynamicLogics
      CADLE.clearCache domain
      return (True, Just version)
      where
        mkAppDynamicLogicElement :: Int -> A.Value -> Int -> UTCTime -> DTADLE.AppDynamicLogicElement
        mkAppDynamicLogicElement version logic order now =
          DTADLE.AppDynamicLogicElement
            { createdAt = now,
              updatedAt = now,
              description = req.description,
              merchantId = mbMerchantId,
              patchedElement = Nothing,
              ..
            }
    verifyOutput :: Value -> [String]
    verifyOutput respResult = do
      case (A.fromJSON respResult :: A.Result b) of
        A.Success _ -> []
        A.Error err -> [show err]

verifyAndUpdateUIDynamicLogic ::
  forall m r a b.
  (BeamFlow m r, ToJSON a, FromJSON b, Show a) =>
  Maybe (Id Lib.Yudhishthira.Types.Merchant) ->
  Proxy b ->
  Text ->
  Lib.Yudhishthira.Types.AppDynamicLogicReq ->
  a ->
  BaseUrl ->
  m Lib.Yudhishthira.Types.AppDynamicLogicResp
verifyAndUpdateUIDynamicLogic mbMerchantId proxy referralLinkPassword req logicData url = do
  resp <- runLogics req.rules logicData
  validateInputData <-
    case (fromJSON resp.result :: Result (LYT.Config Value)) of
      A.Success dpc'' -> pure (dpc''.config)
      A.Error e -> do
        throwError $ InvalidRequest $ "Error in applying dynamic logic: " <> show e
  configValidateResp <- CPF.configValidate url validateInputData
  case configValidateResp.status of
    CPT.VALID_CONFIG -> pure ()
    CPT.INVALID_CONFIG -> throwError $ InvalidRequest "Invalid config"
    CPT.INVALID_REQUEST -> throwError $ InvalidRequest "Invalid request"
  verifyAndUpdateDynamicLogic mbMerchantId proxy referralLinkPassword req logicData

getAppDynamicLogicForDomain :: BeamFlow m r => Maybe Int -> Lib.Yudhishthira.Types.LogicDomain -> m [Lib.Yudhishthira.Types.GetLogicsResp]
getAppDynamicLogicForDomain mbVersion domain = do
  case mbVersion of
    Just version -> do
      logicsObject <- CADLE.findByDomainAndVersion domain version
      let logics = map (.logic) logicsObject
      let description = (listToMaybe logicsObject) >>= (.description)
      return $ [Lib.Yudhishthira.Types.GetLogicsResp domain version description logics]
    Nothing -> do
      allDomainLogics <- CADLE.findByDomain domain
      let sortedLogics = sortByOrderAndVersion allDomainLogics
      let groupedLogics = DLNE.groupBy ((==) `on` (.version)) sortedLogics
      let versionWithLogics = mapMaybe combineLogic groupedLogics
      return $ (versionWithLogics <&> (\(version, description, logics) -> Lib.Yudhishthira.Types.GetLogicsResp domain version description logics))
  where
    sortByOrderAndVersion :: [DTADLE.AppDynamicLogicElement] -> [DTADLE.AppDynamicLogicElement]
    sortByOrderAndVersion = sortBy compareData
      where
        compareData a b =
          case compare b.version a.version of -- Descending for version
            EQ -> compare a.order b.order -- Ascending for order
            cmp -> cmp

    combineLogic :: NonEmpty DTADLE.AppDynamicLogicElement -> Maybe (Int, Maybe Text, [A.Value])
    combineLogic logicElements =
      let logics = DLNE.map (.logic) logicElements -- Combine all logic values into a list
          firstElement = DLNE.head logicElements
       in Just (firstElement.version, firstElement.description, DLNE.toList logics)

getTimeBounds :: BeamFlow m r => Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m Lib.Yudhishthira.Types.TimeBoundResp
getTimeBounds merchantOpCityId domain = do
  allTimeBounds <- CQTBC.findByCityAndDomain merchantOpCityId domain
  return $ (\TimeBoundConfig {..} -> Lib.Yudhishthira.Types.CreateTimeBoundRequest {..}) <$> allTimeBounds

createTimeBounds :: BeamFlow m r => Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.CreateTimeBoundRequest -> m Kernel.Types.APISuccess.APISuccess
createTimeBounds merchantOpCityId req = do
  when (req.timeBounds == Unbounded) $ throwError $ InvalidRequest "Unbounded time bounds not allowed"
  allTimeBounds <- CQTBC.findByCityAndDomain merchantOpCityId req.timeBoundDomain
  forM_ allTimeBounds $ \existingTimeBound -> do
    when (timeBoundsOverlap existingTimeBound.timeBounds req.timeBounds) $ do
      throwError (InvalidRequest $ "Time bounds overlap with existing time bound: " <> existingTimeBound.name)
  now <- getCurrentTime
  let timeBound = mkTimeBound merchantOpCityId now req
  CQTBC.create timeBound
  CQTBC.clearCache merchantOpCityId timeBound.timeBoundDomain timeBound.name
  return Kernel.Types.APISuccess.Success
  where
    mkTimeBound :: Id Lib.Yudhishthira.Types.MerchantOperatingCity -> UTCTime -> Lib.Yudhishthira.Types.CreateTimeBoundRequest -> TimeBoundConfig
    mkTimeBound merchantOperatingCityId now Lib.Yudhishthira.Types.CreateTimeBoundRequest {..} =
      TimeBoundConfig
        { createdAt = now,
          updatedAt = now,
          ..
        }

deleteTimeBounds :: BeamFlow m r => Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> Text -> m Kernel.Types.APISuccess.APISuccess
deleteTimeBounds merchantOpCityId domain name = do
  CQTBC.delete merchantOpCityId domain name
  CQTBC.clearCache merchantOpCityId domain name
  return Kernel.Types.APISuccess.Success

getLogicRollout :: BeamFlow m r => Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Maybe Text -> Lib.Yudhishthira.Types.LogicDomain -> m [Lib.Yudhishthira.Types.LogicRolloutObject]
getLogicRollout merchantOpCityId _ domain = do
  allDomainRollouts <- CADLR.findByMerchantOpCityAndDomain merchantOpCityId domain
  let sortedDomainRollouts = sortBy (\a b -> compare b.timeBounds a.timeBounds) allDomainRollouts
  let groupedRollouts = DLNE.groupBy ((==) `on` (.timeBounds)) sortedDomainRollouts
  return $ mapMaybe combineRollout groupedRollouts
  where
    combineRollout :: NonEmpty AppDynamicLogicRollout -> Maybe Lib.Yudhishthira.Types.LogicRolloutObject
    combineRollout logicRollouts =
      let rollout = DLNE.map (\r -> Lib.Yudhishthira.Types.RolloutVersion r.version r.percentageRollout r.versionDescription) logicRollouts
          firstElement = DLNE.head logicRollouts
       in Just $ Lib.Yudhishthira.Types.LogicRolloutObject firstElement.domain firstElement.timeBounds (DLNE.toList rollout) (firstElement.modifiedBy)

getFrontendLogicUrlAndToken :: BeamFlow m r => m (BaseUrl, Text)
getFrontendLogicUrlAndToken = do
  config <- liftIO (Se.lookupEnv "FRONTEND_LOGIC_URL") >>= fromMaybeM (InvalidRequest "Frontend logic url not found")
  token <- liftIO (Se.lookupEnv "FRONTEND_LOGIC_TOKEN") >>= fromMaybeM (InvalidRequest "Frontend logic token not found")
  url <- parseBaseUrl (T.pack config)
  return (url, T.pack token)

callWebHook :: BeamFlow m r => CPFC.ConfigPilotFrontendReq -> m CPFC.ConfigPilotFrontendRes
callWebHook req = do
  (url, token) <- getFrontendLogicUrlAndToken
  let cfg = CPFC.ConfigPilotFrontendConfig url token
  CPFC.callConfigPilotFrontend req cfg

-- This updates on going release flag in TS Service
callTheFrontEndHook ::
  BeamFlow m r =>
  Kernel.Types.Beckn.Context.City ->
  Lib.Yudhishthira.Types.LogicDomain ->
  m Kernel.Types.APISuccess.APISuccess
callTheFrontEndHook opCity domain = do
  case domain of
    Lib.Yudhishthira.Types.UI_DRIVER os pt -> do
      let req = makeReq opCity os pt
      res <- callWebHook req
      logDebug $ "Response from Frontend Logic: " <> show res
      return Kernel.Types.APISuccess.Success
    Lib.Yudhishthira.Types.UI_RIDER os pt -> do
      let req = makeReq opCity os pt
      res <- callWebHook req
      logDebug $ "Response from Frontend Logic: " <> show res
      return Kernel.Types.APISuccess.Success
    _ -> return Kernel.Types.APISuccess.Success
  where
    makeReq city os pt =
      CPFC.ConfigPilotFrontendReq
        { city = T.pack (show city),
          os = T.pack (show os),
          platform = T.pack (show pt),
          isOnGoingRelease = True
        }

upsertLogicRollout ::
  (BeamFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Maybe (Id Lib.Yudhishthira.Types.Merchant) ->
  Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
  [Lib.Yudhishthira.Types.LogicRolloutObject] ->
  (LYT.LogicDomain -> Id LYT.MerchantOperatingCity -> Id Lib.Yudhishthira.Types.Merchant -> Kernel.Types.Beckn.Context.City -> m LYT.TableDataResp) ->
  Kernel.Types.Beckn.Context.City ->
  m Kernel.Types.APISuccess.APISuccess
upsertLogicRollout mbMerchantId merchantOpCityId rolloutReq giveConfigs opCity = do
  unless (checkSameDomainDifferentTimeBounds rolloutReq) $ throwError $ InvalidRequest "Only domain and different time bounds are allowed"
  domain <- getDomain & fromMaybeM (InvalidRequest "Domain not found")
  now <- getCurrentTime
  if isDriverOrRiderConfig domain || isUIConfig domain
    then do
      version <- handleDriverOrRiderConfigOrUiConfig domain merchantOpCityId now rolloutReq
      fork "Pushing Config History" $ do
        when (isNothing mbMerchantId) $ throwError $ InternalError "Merchant not found"
        configsJson <- (.configs) <$> giveConfigs domain (cast merchantOpCityId) (fromJust mbMerchantId) opCity
        pushConfigHistory domain version merchantOpCityId configsJson
      when (isUIConfig domain) $ do
        void $ callTheFrontEndHook opCity domain -- will update ongoing release in ts service
      return Kernel.Types.APISuccess.Success
    else handleOtherDomain merchantOpCityId now rolloutReq domain
  where
    getDomain :: Maybe Lib.Yudhishthira.Types.LogicDomain
    getDomain = listToMaybe $ map (.domain) rolloutReq

    -- Driver or Rider Config Domain Handling
    handleDriverOrRiderConfigOrUiConfig :: BeamFlow m r => Lib.Yudhishthira.Types.LogicDomain -> Id Lib.Yudhishthira.Types.MerchantOperatingCity -> UTCTime -> [Lib.Yudhishthira.Types.LogicRolloutObject] -> m Int
    handleDriverOrRiderConfigOrUiConfig domain merchantOpCityId' now rolloutReq' = do
      configRolloutObjectsArr <- mapM (mkAppDynamicLogicRolloutDomain merchantOpCityId' now) rolloutReq'
      let configRolloutObjects = concat configRolloutObjectsArr
      when (length configRolloutObjects > 1) $ throwError $ InvalidRequest "Only one version can be set to experiment at a time"
      let mbConfigRolloutObject = listToMaybe configRolloutObjects
      when (isNothing mbConfigRolloutObject) $ throwError $ InvalidRequest $ "No rollout config found for " <> show domain
      let configRolloutObject = fromJust mbConfigRolloutObject
      mbBaseRollout <- CADLR.findBaseRolloutByMerchantOpCityAndDomain merchantOpCityId' configRolloutObject.domain
      baseRollout <- case mbBaseRollout of
        Just baseRollout -> return baseRollout
        Nothing -> do
          newVersion <- do
            latestElement <- QADLE.findLatestVersion (Just 1) Nothing domain
            return (maybe 1 ((+ 1) . (.version)) (listToMaybe latestElement))
          CADLE.create $ mkBaseAppDynamicLogicElement newVersion baseElementPatch 0 now domain
          CADLE.clearCache domain
          let baseRollout = mkBaseAppDynamicLogicRollout newVersion now domain 100
          CADLR.create baseRollout
          CADLR.clearCache (cast merchantOpCityId') domain
          return baseRollout
      mbConfigRolloutObjectDB <- LYSQADLR.findByPrimaryKey configRolloutObject.domain merchantOpCityId configRolloutObject.timeBounds configRolloutObject.version
      handleRolloutUpdate configRolloutObject baseRollout mbConfigRolloutObjectDB merchantOpCityId' domain

    handleRolloutUpdate :: BeamFlow m r => AppDynamicLogicRollout -> AppDynamicLogicRollout -> Maybe AppDynamicLogicRollout -> Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m Int
    handleRolloutUpdate configRolloutObject baseRollout mbConfigRolloutObjectDB merchantOpCityId' domain = case mbConfigRolloutObjectDB of
      Just configRolloutObjectDB -> do
        let originalRolloutPercentage = configRolloutObjectDB.percentageRollout
        when ((baseRollout.percentageRollout - (configRolloutObject.percentageRollout - originalRolloutPercentage)) < 0) $ throwError $ InvalidRequest $ "Insufficient percentage on base version: " <> show baseRollout.percentageRollout <> " either conclude existing versions or reduce rollout percentage."
        when (configRolloutObjectDB.version == baseRollout.version) $ throwError $ InvalidRequest "Percentage of base version cannot be updated directly"
        when (configRolloutObjectDB.experimentStatus /= Just Lib.Yudhishthira.Types.RUNNING) $ throwError $ InvalidRequest "You can only update the percentage of a running experiment"
        let updatedBaseRollout = updateBaseRolloutPercentage baseRollout configRolloutObject originalRolloutPercentage
        LYSQADLR.updateByPrimaryKey updatedBaseRollout
        LYSQADLR.updateByPrimaryKey configRolloutObject
        CADLR.clearCache (cast merchantOpCityId') domain
        return updatedBaseRollout.version
      Nothing -> createNewRollout configRolloutObject baseRollout merchantOpCityId' domain

    updateBaseRolloutPercentage :: AppDynamicLogicRollout -> AppDynamicLogicRollout -> Int -> AppDynamicLogicRollout
    updateBaseRolloutPercentage baseRollout configRolloutObject originalRolloutPercentage = do
      if configRolloutObject.percentageRollout > originalRolloutPercentage
        then baseRollout {percentageRollout = baseRollout.percentageRollout - (configRolloutObject.percentageRollout - originalRolloutPercentage)}
        else baseRollout {percentageRollout = baseRollout.percentageRollout + (originalRolloutPercentage - configRolloutObject.percentageRollout)}

    createNewRollout :: BeamFlow m r => AppDynamicLogicRollout -> AppDynamicLogicRollout -> Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m Int
    createNewRollout configRolloutObject baseRollout merchantOpCityId' domain = do
      when (configRolloutObject.percentageRollout > baseRollout.percentageRollout) $
        throwError $ InvalidRequest $ "Insufficient percentage on base version: " <> show baseRollout.percentageRollout <> " either conclude existing versions or reduce rollout percentage."
      let updatedBaseRollout = baseRollout {percentageRollout = baseRollout.percentageRollout - configRolloutObject.percentageRollout}
      LYSQADLR.updateByPrimaryKey updatedBaseRollout
      CADLR.create configRolloutObject
      CADLR.clearCache (cast merchantOpCityId') domain
      return configRolloutObject.version

    -- Non-driver/rider config domain handling
    handleOtherDomain :: BeamFlow m r => Id Lib.Yudhishthira.Types.MerchantOperatingCity -> UTCTime -> [Lib.Yudhishthira.Types.LogicRolloutObject] -> Lib.Yudhishthira.Types.LogicDomain -> m Kernel.Types.APISuccess.APISuccess
    handleOtherDomain merchantOpCityId' now rolloutReq' domain = do
      rolloutObjectsArr <- mapM (mkAppDynamicLogicRolloutDomain merchantOpCityId' now) rolloutReq'
      let rolloutObjects = concat rolloutObjectsArr
      CADLR.delete (cast merchantOpCityId') domain
      CADLR.createMany rolloutObjects
      CADLR.clearCache (cast merchantOpCityId') domain
      return Kernel.Types.APISuccess.Success

    mkAppDynamicLogicRolloutDomain :: BeamFlow m r => Id Lib.Yudhishthira.Types.MerchantOperatingCity -> UTCTime -> Lib.Yudhishthira.Types.LogicRolloutObject -> m [AppDynamicLogicRollout]
    mkAppDynamicLogicRolloutDomain merchantOperatingCityId now Lib.Yudhishthira.Types.LogicRolloutObject {..} = do
      when (timeBounds /= "Unbounded") $
        void $ CQTBC.findByPrimaryKey merchantOperatingCityId timeBounds domain >>= fromMaybeM (InvalidRequest $ "Time bound not found: " <> timeBounds)
      unless (isDriverOrRiderConfig domain || isUIConfig domain) $ do
        let rolloutSum = sum $ map (.rolloutPercentage) rollout
        when (rolloutSum /= 100) $ throwError $ InvalidRequest "Sum of rollout percentage should be 100"
      mapM (mkAppDynamicLogicRollout merchantOperatingCityId now domain timeBounds (if isDriverOrRiderConfig domain || isUIConfig domain then Just Lib.Yudhishthira.Types.RUNNING else Nothing) modifiedBy) rollout

    mkAppDynamicLogicRollout ::
      BeamFlow m r =>
      Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
      UTCTime ->
      Lib.Yudhishthira.Types.LogicDomain ->
      Text ->
      Maybe Lib.Yudhishthira.Types.ExperimentStatus ->
      Maybe (Id Lib.Yudhishthira.Types.Person) ->
      Lib.Yudhishthira.Types.RolloutVersion ->
      m AppDynamicLogicRollout
    mkAppDynamicLogicRollout merchantOperatingCityId now domain timeBounds mbStatus mbModifiedBy Lib.Yudhishthira.Types.RolloutVersion {..} = do
      logicsObject <- CADLE.findByDomainAndVersion domain version
      let _versionDescription = listToMaybe logicsObject >>= (.description)
      when (null logicsObject) $ throwError $ InvalidRequest $ "Logic not found for version: " <> show version
      return $
        AppDynamicLogicRollout
          { createdAt = now,
            updatedAt = now,
            percentageRollout = rolloutPercentage,
            merchantOperatingCityId = cast merchantOperatingCityId,
            versionDescription = versionDescription <|> _versionDescription,
            merchantId = mbMerchantId,
            modifiedBy = mbModifiedBy,
            experimentStatus = mbStatus,
            isBaseVersion = Nothing,
            ..
          }

    mkBaseAppDynamicLogicElement :: Int -> A.Value -> Int -> UTCTime -> Lib.Yudhishthira.Types.LogicDomain -> DTADLE.AppDynamicLogicElement
    mkBaseAppDynamicLogicElement version logic order now domain' =
      DTADLE.AppDynamicLogicElement
        { createdAt = now,
          updatedAt = now,
          merchantId = mbMerchantId,
          domain = domain',
          description = Nothing,
          patchedElement = Nothing,
          ..
        }
    mkBaseAppDynamicLogicRollout :: Int -> UTCTime -> Lib.Yudhishthira.Types.LogicDomain -> Int -> AppDynamicLogicRollout
    mkBaseAppDynamicLogicRollout newVersion now domain' rolloutPerc =
      AppDynamicLogicRollout
        { domain = domain',
          experimentStatus = Just Lib.Yudhishthira.Types.CONCLUDED,
          isBaseVersion = Just True,
          merchantId = mbMerchantId,
          merchantOperatingCityId = merchantOpCityId,
          modifiedBy = Nothing,
          percentageRollout = rolloutPerc,
          timeBounds = "Unbounded",
          version = newVersion,
          versionDescription = Just "System generated base rollout",
          createdAt = now,
          updatedAt = now,
          ..
        }

    checkSameDomainDifferentTimeBounds :: [Lib.Yudhishthira.Types.LogicRolloutObject] -> Bool
    checkSameDomainDifferentTimeBounds [] = True
    checkSameDomainDifferentTimeBounds (x : xs) =
      all (\obj -> obj.domain == x.domain) xs && length timeBoundsList == length (nub timeBoundsList)
      where
        timeBoundsList = map (.timeBounds) (x : xs)

getAppDynamicLogicVersions :: BeamFlow m r => Maybe Int -> Maybe Int -> Lib.Yudhishthira.Types.LogicDomain -> m Lib.Yudhishthira.Types.AppDynamicLogicVersionResp
getAppDynamicLogicVersions mbLimit mbOffset domain_ = do
  elements <- QADLE.findLatestVersion mbLimit mbOffset domain_
  let groupedElements = DLNE.groupBy ((==) `on` (.version)) elements
  let firstElementArr = map DLNE.head groupedElements
  return $ (\DTADLE.AppDynamicLogicElement {..} -> Lib.Yudhishthira.Types.AppDynamicLogicVersion {..}) <$> firstElementArr

getNammaTagQueryAll :: BeamFlow m r => Lib.Yudhishthira.Types.Chakra -> m Lib.Yudhishthira.Types.ChakraQueryResp
getNammaTagQueryAll chakra_ = do
  chakraQueries <- QChakraQueries.findAllByChakra chakra_
  return $ (\LYTCQ.ChakraQueries {..} -> Lib.Yudhishthira.Types.ChakraQueriesAPIEntity {..}) <$> chakraQueries

groupByTimeBounds :: [AppDynamicLogicRollout] -> [[AppDynamicLogicRollout]]
groupByTimeBounds = groupBy sameTimeBounds

sameTimeBounds :: AppDynamicLogicRollout -> AppDynamicLogicRollout -> Bool
sameTimeBounds r1 r2 = Lib.Yudhishthira.Types.AppDynamicLogicRollout.timeBounds r1 == Lib.Yudhishthira.Types.AppDynamicLogicRollout.timeBounds r2

getNammaTagConfigPilotAllConfigs ::
  BeamFlow m r =>
  Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
  Maybe Bool ->
  Lib.Yudhishthira.Types.ConfigTypeChoice ->
  m [Lib.Yudhishthira.Types.ConfigType]
getNammaTagConfigPilotAllConfigs merchantOpCityId mbUnderExp configChoice = do
  case mbUnderExp of
    Just True -> do
      allRollouts <- CADLR.fetchAllConfigsByMerchantOpCityId merchantOpCityId
      let configRollouts =
            filter
              ( \rollout -> case configChoice of
                  Lib.Yudhishthira.Types.DriverCfg -> case rollout.domain of
                    Lib.Yudhishthira.Types.DRIVER_CONFIG _ -> True
                    _ -> False
                  Lib.Yudhishthira.Types.RiderCfg -> case rollout.domain of
                    Lib.Yudhishthira.Types.RIDER_CONFIG _ -> True
                    _ -> False
              )
              allRollouts
          configsInExperiment :: [Lib.Yudhishthira.Types.LogicDomain] =
            nub $
              map (.domain) $ filter (\rollout -> rollout.percentageRollout /= 100 && rollout.isBaseVersion == Just True) configRollouts
          configTypes = case configChoice of
            Lib.Yudhishthira.Types.DriverCfg -> map extractDriverConfig configsInExperiment
            Lib.Yudhishthira.Types.RiderCfg -> map extractRiderConfig configsInExperiment
      return $ catMaybes configTypes
    _ -> do
      let configTypes :: [Lib.Yudhishthira.Types.ConfigType] = Lib.Yudhishthira.Types.allValues
      return configTypes

getNammaTagConfigPilotAllUiConfigs :: BeamFlow m r => Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Maybe Bool -> Lib.Yudhishthira.Types.ConfigTypeChoice -> m [Lib.Yudhishthira.Types.LogicDomain]
getNammaTagConfigPilotAllUiConfigs merchantOpCityId mbUnderExp configChoice = do
  case mbUnderExp of
    Just True -> do
      allRollouts <- CADLR.fetchAllConfigsByMerchantOpCityId merchantOpCityId
      let configRollouts =
            filter
              ( \rollout -> case configChoice of
                  Lib.Yudhishthira.Types.DriverCfg -> case rollout.domain of
                    Lib.Yudhishthira.Types.UI_DRIVER _ _ -> True
                    _ -> False
                  Lib.Yudhishthira.Types.RiderCfg -> case rollout.domain of
                    Lib.Yudhishthira.Types.UI_RIDER _ _ -> True
                    _ -> False
              )
              allRollouts
          configsInExperiment :: [Lib.Yudhishthira.Types.LogicDomain] =
            nub $
              map (.domain) $ filter (\rollout -> rollout.percentageRollout /= 100 && rollout.isBaseVersion == Just True) configRollouts
      return configsInExperiment
    _ -> do
      let configTypes :: [Lib.Yudhishthira.Types.LogicDomain] = filter isUIConfig Lib.Yudhishthira.Types.allValues
      return configTypes

getNammaTagConfigPilotConfigDetails :: BeamFlow m r => Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m [Lib.Yudhishthira.Types.ConfigDetailsResp]
getNammaTagConfigPilotConfigDetails merchantOpCityId domain' = do
  allConfigRollouts <- CADLR.findByMerchantOpCityAndDomain merchantOpCityId domain'
  let runningConfigRollouts = filter (\rollout -> rollout.isBaseVersion == Just True || rollout.percentageRollout /= 0) allConfigRollouts
  mapM makeConfigDetailResp runningConfigRollouts
  where
    makeConfigDetailResp :: BeamFlow m r => AppDynamicLogicRollout -> m Lib.Yudhishthira.Types.ConfigDetailsResp
    makeConfigDetailResp (AppDynamicLogicRollout {..}) = do
      logicsObject <- CADLE.findByDomainAndVersion domain' version
      let logics = map (.logic) logicsObject
      return $
        Lib.Yudhishthira.Types.ConfigDetailsResp
          { modifiedBy = modifiedBy,
            percentageRollout = percentageRollout,
            version = version,
            configPatch = logics,
            isBasePatch = isBaseVersion == Just True
          }

getNammaTagConfigPilotUiConfigDetails :: BeamFlow m r => Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m [Lib.Yudhishthira.Types.ConfigDetailsResp]
getNammaTagConfigPilotUiConfigDetails merchantOpCityId domain' = do
  allRollouts <- CADLR.fetchAllConfigsByMerchantOpCityId merchantOpCityId
  let configRollouts = filter (\rollout -> rollout.domain == domain') allRollouts
  mapM makeConfigDetailResp configRollouts
  where
    makeConfigDetailResp :: BeamFlow m r => AppDynamicLogicRollout -> m Lib.Yudhishthira.Types.ConfigDetailsResp
    makeConfigDetailResp (AppDynamicLogicRollout {..}) = do
      logicsObject <- CADLE.findByDomainAndVersion domain' version
      let logics = map (.logic) logicsObject
      return $
        Lib.Yudhishthira.Types.ConfigDetailsResp
          { modifiedBy = modifiedBy,
            percentageRollout = percentageRollout,
            version = version,
            configPatch = logics,
            isBasePatch = isBaseVersion == Just True
          }

postNammaTagConfigPilotActionChange :: (BeamFlow m r, EsqDBFlow m r, CacheFlow m r) => Maybe (Id LYT.Merchant) -> Id LYT.MerchantOperatingCity -> LYT.ActionChangeRequest -> (Id LYT.MerchantOperatingCity -> LYT.ConcludeReq -> [A.Value] -> Maybe (Id LYT.Merchant) -> Kernel.Types.Beckn.Context.City -> m ()) -> (LYT.LogicDomain -> Id LYT.MerchantOperatingCity -> Id Lib.Yudhishthira.Types.Merchant -> Kernel.Types.Beckn.Context.City -> m LYT.TableDataResp) -> Kernel.Types.Beckn.Context.City -> m Kernel.Types.APISuccess.APISuccess
postNammaTagConfigPilotActionChange mbMerchantId merchantOpCityId req handleConfigDBUpdate' giveConfigs opCity = do
  case req of
    LYT.Conclude concludeReq -> do
      expRollout <- LYSQADLR.findByPrimaryKey concludeReq.domain (cast merchantOpCityId) "Unbounded" concludeReq.version >>= fromMaybeM (InvalidRequest $ "Rollout not found for Domain: " <> show concludeReq.domain <> " City: " <> show merchantOpCityId <> " TimeBounds: " <> "Unbounded" <> " Version: " <> show concludeReq.version)
      baseRollout <- CADLR.findBaseRolloutByMerchantOpCityAndDomain (cast merchantOpCityId) concludeReq.domain >>= fromMaybeM (InvalidRequest "Base Rollout not found")
      when (concludeReq.version == baseRollout.version) $ throwError $ InvalidRequest "Cannot conclude the base rollout"
      when (expRollout.experimentStatus /= Just LYT.RUNNING) $ throwError $ InvalidRequest "The experiment should be in running state for getting concluded"
      baseElements <- CADLE.findByDomainAndVersion concludeReq.domain baseRollout.version
      let baseLogics = fmap (.logic) baseElements
      handleConfigDBUpdate' (cast merchantOpCityId) concludeReq baseLogics mbMerchantId opCity
      let originalBasePercentage = baseRollout.percentageRollout
          updatedBaseRollout =
            baseRollout
              { isBaseVersion = Nothing,
                experimentStatus = Just LYT.CONCLUDED,
                percentageRollout = 0
              }
      LYSQADLR.updateByPrimaryKey updatedBaseRollout
      let concludedRollout =
            expRollout
              { experimentStatus = Just LYT.CONCLUDED,
                isBaseVersion = Just True,
                percentageRollout = expRollout.percentageRollout + originalBasePercentage
              }
      LYSQADLR.updateByPrimaryKey concludedRollout
      CADLR.clearCache (cast merchantOpCityId) concludeReq.domain
      makeConfigHistory concludeReq.domain concludeReq.version
    LYT.Abort abortReq -> do
      expRollout <- LYSQADLR.findByPrimaryKey abortReq.domain merchantOpCityId "Unbounded" abortReq.version >>= fromMaybeM (InvalidRequest $ "Rollout not found for Domain: " <> show abortReq.domain <> " City: " <> show merchantOpCityId <> " TimeBounds: " <> "Unbounded" <> " Version: " <> show abortReq.version)
      mbBaseRollout <- CADLR.findBaseRolloutByMerchantOpCityAndDomain merchantOpCityId abortReq.domain
      when (isNothing mbBaseRollout) $ throwError $ InvalidRequest $ "No Base version rollout found for " <> show abortReq.domain
      let baseRollout = fromJust mbBaseRollout
          updatedBaseRollout =
            baseRollout
              { percentageRollout = baseRollout.percentageRollout + expRollout.percentageRollout
              }
          abortedRollout =
            expRollout
              { percentageRollout = 0,
                experimentStatus = Just LYT.DISCARDED
              }
      when (abortReq.version == baseRollout.version) $ throwError $ InvalidRequest "Cannot abort the base rollout"
      when (expRollout.experimentStatus /= Just LYT.RUNNING) $ throwError $ InvalidRequest "The experiment should be in running state for getting aborted"
      LYSQADLR.updateByPrimaryKey abortedRollout
      LYSQADLR.updateByPrimaryKey updatedBaseRollout
      CADLR.clearCache (cast merchantOpCityId) abortReq.domain
      makeConfigHistory abortReq.domain abortReq.version
    Lib.Yudhishthira.Types.Revert revertReq -> do
      baseRollout <- CADLR.findBaseRolloutByMerchantOpCityAndDomain merchantOpCityId revertReq.domain >>= fromMaybeM (InvalidRequest $ "No Base version rollout found for " <> show revertReq.domain)
      now <- getCurrentTime
      newVersion <- do
        latestElement <- QADLE.findLatestVersion (Just 1) Nothing revertReq.domain
        return (maybe 1 ((+ 1) . (.version)) (listToMaybe latestElement))
      let newBaseElement = mkBaseAppDynamicLogicElement newVersion baseElementPatch 0 now revertReq.domain
          newBaseRollout = mkBaseAppDynamicLogicRollout newVersion baseRollout now revertReq.domain
          revertedBaseRollout =
            baseRollout
              { isBaseVersion = Nothing,
                percentageRollout = 0,
                experimentStatus = Just LYT.REVERTED
              }
      LYSQADLR.updateByPrimaryKey revertedBaseRollout
      CADLE.create newBaseElement
      CADLR.create newBaseRollout
      CADLE.clearCache revertReq.domain
      CADLR.clearCache (cast merchantOpCityId) revertReq.domain
      makeConfigHistory revertReq.domain baseRollout.version
  pure Kernel.Types.APISuccess.Success
  where
    makeConfigHistory domain version = do
      fork "pushing config history" $ do
        when (isNothing mbMerchantId) $ throwError $ InternalError "Merchant not found"
        configsJson <- (.configs) <$> giveConfigs domain (cast merchantOpCityId) (fromJust mbMerchantId) opCity
        case req of
          LYT.Abort _ -> do
            pushConfigHistory domain version merchantOpCityId configsJson
          _ -> do
            allRollouts <- CADLR.fetchAllConfigsByMerchantOpCityId merchantOpCityId
            let configRollouts = filter (\rollout -> rollout.domain == domain && (rollout.percentageRollout /= 0 || rollout.isBaseVersion == Just True)) allRollouts
            mapM_ (\rollout -> pushConfigHistory domain rollout.version merchantOpCityId configsJson) configRollouts

    mkBaseAppDynamicLogicElement :: Int -> A.Value -> Int -> UTCTime -> LYT.LogicDomain -> DTADLE.AppDynamicLogicElement
    mkBaseAppDynamicLogicElement version logic order now domain =
      DTADLE.AppDynamicLogicElement
        { createdAt = now,
          updatedAt = now,
          merchantId = mbMerchantId,
          domain = domain,
          description = Nothing,
          patchedElement = Nothing,
          ..
        }
    mkBaseAppDynamicLogicRollout :: Int -> AppDynamicLogicRollout -> UTCTime -> LYT.LogicDomain -> AppDynamicLogicRollout
    mkBaseAppDynamicLogicRollout newVersion originalBaseRollout now domain =
      AppDynamicLogicRollout
        { domain = domain,
          experimentStatus = Just LYT.CONCLUDED,
          isBaseVersion = Just True,
          merchantId = mbMerchantId,
          merchantOperatingCityId = merchantOpCityId,
          modifiedBy = Nothing,
          percentageRollout = originalBaseRollout.percentageRollout,
          timeBounds = "Unbounded",
          version = newVersion,
          versionDescription = Just "System generated base rollout",
          createdAt = now,
          updatedAt = now,
          ..
        }

isString :: A.Value -> Bool
isString (A.String _) = True
isString _ = False

inRange :: A.Value -> (Double, Double) -> Bool
inRange (A.Number num) (rangeStart, rangeEnd) = toRealFloat num >= rangeStart && toRealFloat num <= rangeEnd
inRange _ _ = False

isDriverOrRiderConfig :: Lib.Yudhishthira.Types.LogicDomain -> Bool
isDriverOrRiderConfig (Lib.Yudhishthira.Types.DRIVER_CONFIG _) = True
isDriverOrRiderConfig (Lib.Yudhishthira.Types.RIDER_CONFIG _) = True
isDriverOrRiderConfig _ = False

isUIConfig :: LYT.LogicDomain -> Bool
isUIConfig (LYT.UI_DRIVER _ _) = True
isUIConfig (LYT.UI_RIDER _ _) = True
isUIConfig _ = False

extractDriverConfig :: Lib.Yudhishthira.Types.LogicDomain -> Maybe Lib.Yudhishthira.Types.ConfigType
extractDriverConfig (Lib.Yudhishthira.Types.DRIVER_CONFIG config) = Just config
extractDriverConfig _ = Nothing

extractRiderConfig :: Lib.Yudhishthira.Types.LogicDomain -> Maybe Lib.Yudhishthira.Types.ConfigType
extractRiderConfig (Lib.Yudhishthira.Types.RIDER_CONFIG config) = Just config
extractRiderConfig _ = Nothing

pushConfigHistory :: (BeamFlow m r, EsqDBFlow m r, CacheFlow m r) => LYT.LogicDomain -> Int -> Id LYT.MerchantOperatingCity -> [A.Value] -> m ()
pushConfigHistory domain version merchantOpCityId configsJson = do
  expRollout <- LYSQADLR.findByPrimaryKey domain (cast merchantOpCityId) "Unbounded" version >>= fromMaybeM (InternalError $ "Experiment Rollout not found for domain: " <> show domain <> " and version: " <> show version)
  baseRollout <- CADLR.findBaseRolloutByMerchantOpCityAndDomain (cast merchantOpCityId) domain >>= fromMaybeM (InternalError "Base Rollout not found")
  logicsObject <- CADLE.findByDomainAndVersion domain version
  when (null logicsObject) $ throwError $ InternalError $ "Logics not found for domain: " <> show domain <> " and version: " <> show version
  let logics = map (.logic) logicsObject
  let configWrappers = map (\cfg -> LYT.Config cfg Nothing 0) configsJson
  finalConfigs <- mapM (runLogicsOnConfig logics) configWrappers
  uuid <- generateGUID
  now <- getCurrentTime
  let configHistory =
        LYT.ConfigHistory
          { id = uuid,
            domain,
            version,
            status = expRollout.experimentStatus,
            merchantOperatingCityId = cast merchantOpCityId,
            configJson = finalConfigs,
            baseVersionUsed = baseRollout.version,
            createdAt = now
          }
  pushToKafka configHistory "config-history" ""
  where
    runLogicsOnConfig logics configWrapper = do
      response <- withTryCatch "runLogics:runLogicsOnConfig" $ runLogics logics configWrapper
      case response of
        Left e -> do
          throwError (InternalError $ "Error in push config history for domain: " <> show domain <> " and version: " <> show version <> " and error: " <> show e)
        Right resp -> do
          return resp.result

postNammaTagConfigPilotGetPatchedElement :: BeamFlow m r => Id LYT.MerchantOperatingCity -> LYT.GetPatchedElementReq -> m Lib.Yudhishthira.Types.GetPatchedElementResp
postNammaTagConfigPilotGetPatchedElement _ req = do
  appDynamicLogicElement <- QADLE.findByPrimaryKey req.domain 0 req.version >>= fromMaybeM (InvalidRequest $ "No AppDynamicLogicElement found for domain " <> show req.domain <> " and version " <> show req.version)
  return $ Lib.Yudhishthira.Types.GetPatchedElementResp $ appDynamicLogicElement.patchedElement
