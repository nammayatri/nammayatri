module Lib.Yudhishthira.Flow.Dashboard where

import qualified ConfigPilotFrontend.Common as CPFC
import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Aeson as A
import Data.List (groupBy, nub, sortBy)
import qualified Data.List.NonEmpty as DLNE
import Data.Scientific (toRealFloat)
import qualified Data.Text as T
import JsonLogic
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.TimeBound
import Kernel.Utils.Common
import Lib.Yudhishthira.Event.KaalChakra as KaalChakraEvent
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicElement as CADLE
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout as CADLR
import qualified Lib.Yudhishthira.Storage.CachedQueries.TimeBoundConfig as CQTBC
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicElement as LYSQADLE
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicElement as QADLE
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicRollout as LYSQADLR
import qualified Lib.Yudhishthira.Storage.Queries.ChakraQueries as QChakraQueries
import qualified Lib.Yudhishthira.Storage.Queries.ChakraQueries as SQCQ
import qualified Lib.Yudhishthira.Storage.Queries.NammaTag as QNT
import Lib.Yudhishthira.Tools.Error
import Lib.Yudhishthira.Tools.Utils
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogicElement as DTADLE
import Lib.Yudhishthira.Types.AppDynamicLogicRollout
import qualified Lib.Yudhishthira.Types.ChakraQueries
import qualified Lib.Yudhishthira.Types.ChakraQueries as LYTCQ
import qualified Lib.Yudhishthira.Types.NammaTag as DNT
import Lib.Yudhishthira.Types.TimeBoundConfig
import qualified System.Environment as Se

postTagCreate :: BeamFlow m r => Lib.Yudhishthira.Types.CreateNammaTagRequest -> m Kernel.Types.APISuccess.APISuccess
postTagCreate tagRequest = do
  nammaTag <- buildNammaTag
  checkForDuplicacy nammaTag.name
  QNT.create nammaTag
  pure Kernel.Types.APISuccess.Success
  where
    checkForDuplicacy name = do
      QNT.findByPrimaryKey name >>= \case
        Just _ -> throwError (TagAlreadyExists name)
        Nothing -> pure ()

    buildNammaTag = do
      now <- getCurrentTime
      case tagRequest of
        Lib.Yudhishthira.Types.ApplicationTag Lib.Yudhishthira.Types.NammaTagApplication {..} ->
          return $
            DNT.NammaTag
              { category = tagCategory,
                info = DNT.Application (DNT.ApplicationTagInfo tagStage),
                name = tagName,
                possibleValues = tagPossibleValues,
                rule = tagRule,
                description = description,
                actionEngine = Nothing,
                validity = tagValidity,
                createdAt = now,
                updatedAt = now
              }
        Lib.Yudhishthira.Types.KaalChakraTag Lib.Yudhishthira.Types.NammaTagChakra {..} ->
          return $
            DNT.NammaTag
              { category = tagCategory,
                info = DNT.KaalChakra (DNT.KaalChakraTagInfo tagChakra),
                name = tagName,
                possibleValues = tagPossibleValues,
                rule = tagRule,
                description = description,
                actionEngine,
                validity = tagValidity,
                createdAt = now,
                updatedAt = now
              }
        Lib.Yudhishthira.Types.ManualTag Lib.Yudhishthira.Types.NammaTagManual {..} ->
          return $
            DNT.NammaTag
              { category = tagCategory,
                info = DNT.Manual,
                name = tagName,
                possibleValues = tagPossibleValues,
                rule = Lib.Yudhishthira.Types.LLM "empty-context",
                description = description,
                actionEngine = Nothing,
                validity = tagValidity,
                createdAt = now,
                updatedAt = now
              }

postTagUpdate :: BeamFlow m r => Lib.Yudhishthira.Types.UpdateNammaTagRequest -> m Kernel.Types.APISuccess.APISuccess
postTagUpdate tagRequest = do
  tag <- QNT.findByPrimaryKey tagRequest.tagName >>= fromMaybeM (InvalidRequest "Tag not found in the system, please create the tag")
  updatedTag <- buildUpdateNammaTagEntity tag
  QNT.updateByPrimaryKey updatedTag
  return Kernel.Types.APISuccess.Success
  where
    buildUpdateNammaTagEntity tag = do
      now <- getCurrentTime
      let validity = case tagRequest.resetTagValidity of
            Just True -> Nothing
            _ -> tagRequest.tagValidity <|> tag.validity
      return $
        DNT.NammaTag
          { category = fromMaybe tag.category tagRequest.tagCategory,
            info = buildTagInfo tag,
            name = tag.name,
            possibleValues = fromMaybe tag.possibleValues tagRequest.tagPossibleValues,
            rule = fromMaybe tag.rule tagRequest.tagRule,
            description = tagRequest.description <|> tag.description,
            actionEngine = tagRequest.actionEngine <|> tag.actionEngine,
            validity,
            createdAt = tag.createdAt,
            updatedAt = now
          }
    buildTagInfo tag = do
      case tag.info of
        DNT.Application (DNT.ApplicationTagInfo tagStage) -> DNT.Application (DNT.ApplicationTagInfo (fromMaybe tagStage tagRequest.tagStage))
        DNT.KaalChakra (DNT.KaalChakraTagInfo tagChakra) -> DNT.KaalChakra (DNT.KaalChakraTagInfo (fromMaybe tagChakra tagRequest.tagChakra))
        DNT.Manual -> DNT.Manual

deleteTag :: BeamFlow m r => T.Text -> m Kernel.Types.APISuccess.APISuccess
deleteTag tagName = do
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
          let mbTagValue = textToMaybeValue tagValueText
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
  nammaTags <- QNT.findAllByApplicationEvent event
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
              ..
            }
    verifyOutput :: Value -> [String]
    verifyOutput respResult = do
      case (A.fromJSON respResult :: A.Result b) of
        A.Success _ -> []
        A.Error err -> [show err]

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
       in Just $ Lib.Yudhishthira.Types.LogicRolloutObject firstElement.domain firstElement.timeBounds (DLNE.toList rollout) (firstElement.modifiedBy) firstElement.experimentStatus

getFrontendLogicUrlAndToken :: BeamFlow m r => m (BaseUrl, Text)
getFrontendLogicUrlAndToken = do
  config <- liftIO (Se.lookupEnv "FRONTEND_LOGIC_URL") >>= fromMaybeM (InvalidRequest "Frontend logic url not found")
  token <- liftIO (Se.lookupEnv "FRONTEND_LOGIC_TOKEN") >>= fromMaybeM (InvalidRequest "Frontend logic token not found")
  url <- parseBaseUrl (T.pack config)
  return (url, T.pack token)

callWebHook :: BeamFlow m r => Text -> m CPFC.ConfigPilotFrontendRes
callWebHook domain = do
  let cpfcreq = CPFC.ConfigPilotFrontendReq domain True
  (url, token) <- getFrontendLogicUrlAndToken
  let cfg = CPFC.ConfigPilotFrontendConfig url token
  CPFC.callConfigPilotFrontend cpfcreq cfg

callTheFrontEndHook ::
  BeamFlow m r =>
  Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
  Lib.Yudhishthira.Types.LogicDomain ->
  m Kernel.Types.APISuccess.APISuccess
callTheFrontEndHook merchantOpCityId domain = do
  case domain of
    Lib.Yudhishthira.Types.UI_DRIVER dt pt -> do
      let domain' = "ui_driver:" <> T.pack (show merchantOpCityId) <> ":" <> T.pack (show dt) <> ":" <> T.pack (show pt)
      res <- callWebHook domain'
      logDebug $ "Response from Frontend Logic: " <> show res
      return Kernel.Types.APISuccess.Success
    Lib.Yudhishthira.Types.UI_RIDER dt pt -> do
      let domain' = "ui_customer:" <> T.pack (show merchantOpCityId) <> ":" <> T.pack (show dt) <> ":" <> T.pack (show pt)
      res <- callWebHook domain'
      logDebug $ "Response from Frontend Logic: " <> show res
      return Kernel.Types.APISuccess.Success
    _ -> return Kernel.Types.APISuccess.Success

upsertLogicRollout ::
  BeamFlow m r =>
  Maybe (Id Lib.Yudhishthira.Types.Merchant) ->
  Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
  [Lib.Yudhishthira.Types.LogicRolloutObject] ->
  m Kernel.Types.APISuccess.APISuccess
upsertLogicRollout mbMerchantId merchantOpCityId rolloutReq = do
  unless (checkSameDomainDifferentTimeBounds rolloutReq) $ throwError $ InvalidRequest "Only domain and different time bounds are allowed"
  domain <- getDomain & fromMaybeM (InvalidRequest "Domain not found")
  now <- getCurrentTime
  if isDriverOrRiderConfig domain
    then handleDriverOrRiderConfig domain merchantOpCityId now rolloutReq
    else handleOtherDomain merchantOpCityId now rolloutReq domain
  where
    getDomain :: Maybe Lib.Yudhishthira.Types.LogicDomain
    getDomain = listToMaybe $ map (.domain) rolloutReq

    -- Driver or Rider Config Domain Handling
    handleDriverOrRiderConfig :: BeamFlow m r => Lib.Yudhishthira.Types.LogicDomain -> Id Lib.Yudhishthira.Types.MerchantOperatingCity -> UTCTime -> [Lib.Yudhishthira.Types.LogicRolloutObject] -> m Kernel.Types.APISuccess.APISuccess
    handleDriverOrRiderConfig domain merchantOpCityId' now rolloutReq' = do
      configRolloutObjectsArr <- mapM (mkAppDynamicLogicRolloutDomain merchantOpCityId' now) rolloutReq'
      let configRolloutObjects = concat configRolloutObjectsArr
      when (length configRolloutObjects > 1) $ throwError $ InvalidRequest "Only one version can be set to experiment at a time"
      let mbConfigRolloutObject = listToMaybe configRolloutObjects
      when (isNothing mbConfigRolloutObject) $ throwError $ InvalidRequest $ "No rollout config found for " <> show domain
      let configRolloutObject = fromJust mbConfigRolloutObject
      mbBaseRollout <- LYSQADLR.findByCityAndDomainAndIsBase merchantOpCityId' configRolloutObject.domain
      when (isNothing mbBaseRollout) $ do
        newVersion <- do
          latestElement <- QADLE.findLatestVersion (Just 1) Nothing domain
          return (maybe 1 ((+ 1) . (.version)) (listToMaybe latestElement))
        LYSQADLE.create $ mkBaseAppDynamicLogicElement newVersion baseElementPatch 0 now domain
        LYSQADLR.create $ mkBaseAppDynamicLogicRollout newVersion now domain (100 - configRolloutObject.percentageRollout)
      mbConfigRolloutObjectDB <- LYSQADLR.findByPrimaryKey configRolloutObject.domain merchantOpCityId configRolloutObject.timeBounds configRolloutObject.version
      handleBaseRolloutUpdate configRolloutObject mbBaseRollout mbConfigRolloutObjectDB merchantOpCityId' domain

    handleBaseRolloutUpdate :: BeamFlow m r => AppDynamicLogicRollout -> Maybe AppDynamicLogicRollout -> Maybe AppDynamicLogicRollout -> Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m Kernel.Types.APISuccess.APISuccess
    handleBaseRolloutUpdate configRolloutObject mbBaseRollout mbConfigRolloutObjectDB merchantOpCityId' domain = case mbConfigRolloutObjectDB of
      Just configRolloutObjectDB -> do
        let baseRollout = fromJust mbBaseRollout
            originalRolloutPercentage = configRolloutObjectDB.percentageRollout
        when ((baseRollout.percentageRollout - (configRolloutObject.percentageRollout - originalRolloutPercentage)) < 0) $ throwError $ InvalidRequest $ "Insufficient percentage on base version: " <> show baseRollout.percentageRollout <> " either conclude existing versions or reduce rollout percentage."
        when (configRolloutObjectDB.version == baseRollout.version) $ throwError $ InvalidRequest "Percentage of base version cannot be updated directly"
        when (configRolloutObjectDB.experimentStatus /= Just Lib.Yudhishthira.Types.RUNNING) $ throwError $ InvalidRequest "You can only update the percentage of a running experiment"
        let updatedBaseRollout = updateBaseRolloutPercentage baseRollout configRolloutObject originalRolloutPercentage
        LYSQADLR.updateByPrimaryKey updatedBaseRollout
        LYSQADLR.updateByPrimaryKey configRolloutObject
        CADLR.clearDomainCache (cast merchantOpCityId') domain
        return Kernel.Types.APISuccess.Success
      Nothing -> createNewRollout configRolloutObject mbBaseRollout merchantOpCityId' domain

    updateBaseRolloutPercentage :: AppDynamicLogicRollout -> AppDynamicLogicRollout -> Int -> AppDynamicLogicRollout
    updateBaseRolloutPercentage baseRollout configRolloutObject originalRolloutPercentage = do
      if configRolloutObject.percentageRollout > originalRolloutPercentage
        then baseRollout {percentageRollout = baseRollout.percentageRollout - (configRolloutObject.percentageRollout - originalRolloutPercentage)}
        else baseRollout {percentageRollout = baseRollout.percentageRollout + (originalRolloutPercentage - configRolloutObject.percentageRollout)}

    createNewRollout :: BeamFlow m r => AppDynamicLogicRollout -> Maybe AppDynamicLogicRollout -> Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m Kernel.Types.APISuccess.APISuccess
    createNewRollout configRolloutObject mbBaseRollout merchantOpCityId' domain =
      case mbBaseRollout of
        Just baseRollout -> do
          when (configRolloutObject.percentageRollout > baseRollout.percentageRollout) $
            throwError $ InvalidRequest $ "Insufficient percentage on base version: " <> show baseRollout.percentageRollout <> " either conclude existing versions or reduce rollout percentage."
          let updatedBaseRollout = baseRollout {percentageRollout = baseRollout.percentageRollout - configRolloutObject.percentageRollout}
          LYSQADLR.updateByPrimaryKey updatedBaseRollout
          LYSQADLR.create configRolloutObject
          CADLR.clearDomainCache (cast merchantOpCityId') domain
          CADLR.clearCityConfigsCache (cast merchantOpCityId')
          return Kernel.Types.APISuccess.Success
        Nothing -> do
          LYSQADLR.create configRolloutObject
          CADLR.clearDomainCache (cast merchantOpCityId') domain
          CADLR.clearCityConfigsCache (cast merchantOpCityId')
          return Kernel.Types.APISuccess.Success

    -- Non-driver/rider config domain handling
    handleOtherDomain :: BeamFlow m r => Id Lib.Yudhishthira.Types.MerchantOperatingCity -> UTCTime -> [Lib.Yudhishthira.Types.LogicRolloutObject] -> Lib.Yudhishthira.Types.LogicDomain -> m Kernel.Types.APISuccess.APISuccess
    handleOtherDomain merchantOpCityId' now rolloutReq' domain = do
      rolloutObjectsArr <- mapM (mkAppDynamicLogicRolloutDomain merchantOpCityId' now) rolloutReq'
      let rolloutObjects = concat rolloutObjectsArr
      CADLR.delete (cast merchantOpCityId') domain
      CADLR.createMany rolloutObjects
      CADLR.clearDomainCache (cast merchantOpCityId') domain
      CADLR.clearCityConfigsCache (cast merchantOpCityId')
      return Kernel.Types.APISuccess.Success

    mkAppDynamicLogicRolloutDomain :: BeamFlow m r => Id Lib.Yudhishthira.Types.MerchantOperatingCity -> UTCTime -> Lib.Yudhishthira.Types.LogicRolloutObject -> m [AppDynamicLogicRollout]
    mkAppDynamicLogicRolloutDomain merchantOperatingCityId now Lib.Yudhishthira.Types.LogicRolloutObject {..} = do
      when (timeBounds /= "Unbounded") $
        void $ CQTBC.findByPrimaryKey merchantOperatingCityId timeBounds domain >>= fromMaybeM (InvalidRequest $ "Time bound not found: " <> timeBounds)
      unless (isDriverOrRiderConfig domain) $ do
        let rolloutSum = sum $ map (.rolloutPercentage) rollout
        when (rolloutSum /= 100) $ throwError $ InvalidRequest "Sum of rollout percentage should be 100"
      mapM (mkAppDynamicLogicRollout merchantOperatingCityId now domain timeBounds experimentStatus modifiedBy) rollout

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
          versionDescription = Nothing,
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

getNammaTagConfigPilotAllConfigsProvider :: BeamFlow m r => Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Maybe Bool -> m [Lib.Yudhishthira.Types.ConfigType]
getNammaTagConfigPilotAllConfigsProvider merchantOpCityId mbUnderExp = do
  allRollouts <- CADLR.fetchAllConfigsByMerchantOpCityId merchantOpCityId
  let driverConfigRollouts =
        filter
          ( \rollout -> case rollout.domain of
              Lib.Yudhishthira.Types.DRIVER_CONFIG _ -> True
              _ -> False
          )
          allRollouts
  case mbUnderExp of
    Just True -> do
      let configsInExperiment :: [Lib.Yudhishthira.Types.LogicDomain] = nub $ map (.domain) $ filter (\rollout -> rollout.percentageRollout /= 100 && rollout.isBaseVersion == Just True) allRollouts
          configTypes = map extractDriverConfig configsInExperiment
      return $ catMaybes configTypes
    _ -> do
      let allConfigDomains = nub $ map domain driverConfigRollouts
          configTypes = mapMaybe extractDriverConfig allConfigDomains
      return configTypes
  where
    extractDriverConfig :: Lib.Yudhishthira.Types.LogicDomain -> Maybe Lib.Yudhishthira.Types.ConfigType
    extractDriverConfig (Lib.Yudhishthira.Types.DRIVER_CONFIG config) = Just config
    extractDriverConfig _ = Nothing

getNammaTagConfigPilotAllConfigsRider :: BeamFlow m r => Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Maybe Bool -> m [Lib.Yudhishthira.Types.ConfigType]
getNammaTagConfigPilotAllConfigsRider merchantOpCityId mbUnderExp = do
  allRollouts <- CADLR.fetchAllConfigsByMerchantOpCityId merchantOpCityId
  let riderConfigRollouts =
        filter
          ( \rollout -> case rollout.domain of
              Lib.Yudhishthira.Types.RIDER_CONFIG _ -> True
              _ -> False
          )
          allRollouts
  case mbUnderExp of
    Just True -> do
      let configsInExperiment :: [Lib.Yudhishthira.Types.LogicDomain] = nub $ map (.domain) $ filter (\rollout -> rollout.percentageRollout /= 100 && rollout.isBaseVersion == Just True) allRollouts
          configTypes = map extractRiderConfig configsInExperiment
      return $ catMaybes configTypes
    _ -> do
      let allConfigDomains = nub $ map domain riderConfigRollouts
          configTypes = mapMaybe extractRiderConfig allConfigDomains
      return configTypes
  where
    extractRiderConfig :: Lib.Yudhishthira.Types.LogicDomain -> Maybe Lib.Yudhishthira.Types.ConfigType
    extractRiderConfig (Lib.Yudhishthira.Types.RIDER_CONFIG config) = Just config
    extractRiderConfig _ = Nothing

getNammaTagConfigPilotConfigDetailsProvider :: BeamFlow m r => Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.ConfigType -> m [Lib.Yudhishthira.Types.ConfigDetailsResp]
getNammaTagConfigPilotConfigDetailsProvider merchantOpCityId cfg = do
  allConfigRollouts <- CADLR.findByMerchantOpCityAndDomain merchantOpCityId (Lib.Yudhishthira.Types.DRIVER_CONFIG cfg)
  mapM makeConfigDetailResp allConfigRollouts
  where
    makeConfigDetailResp :: BeamFlow m r => AppDynamicLogicRollout -> m Lib.Yudhishthira.Types.ConfigDetailsResp
    makeConfigDetailResp (AppDynamicLogicRollout {..}) = do
      logicsObject <- CADLE.findByDomainAndVersion (Lib.Yudhishthira.Types.DRIVER_CONFIG cfg) version
      let logics = map (.logic) logicsObject
      return $
        Lib.Yudhishthira.Types.ConfigDetailsResp
          { modifiedBy = modifiedBy,
            percentageRollout = percentageRollout,
            version = version,
            configPatch = logics
          }

getNammaTagConfigPilotConfigDetailsRider :: BeamFlow m r => Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.ConfigType -> m [Lib.Yudhishthira.Types.ConfigDetailsResp]
getNammaTagConfigPilotConfigDetailsRider merchantOpCityId cfg = do
  allConfigRollouts <- CADLR.findByMerchantOpCityAndDomain merchantOpCityId (Lib.Yudhishthira.Types.RIDER_CONFIG cfg)
  mapM makeConfigDetailResp allConfigRollouts
  where
    makeConfigDetailResp :: BeamFlow m r => AppDynamicLogicRollout -> m Lib.Yudhishthira.Types.ConfigDetailsResp
    makeConfigDetailResp (AppDynamicLogicRollout {..}) = do
      logicsObject <- CADLE.findByDomainAndVersion (Lib.Yudhishthira.Types.RIDER_CONFIG cfg) version
      let logics = map (.logic) logicsObject
      return $
        Lib.Yudhishthira.Types.ConfigDetailsResp
          { modifiedBy = modifiedBy,
            percentageRollout = percentageRollout,
            version = version,
            configPatch = logics
          }

postNammaTagConfigPilotActionChange :: BeamFlow m r => Maybe (Id Lib.Yudhishthira.Types.Merchant) -> Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.ActionChangeRequest -> Maybe Int -> m Kernel.Types.APISuccess.APISuccess
postNammaTagConfigPilotActionChange mbMerchantId merchantOpCityId req originalBasePercentage = do
  case req of
    Lib.Yudhishthira.Types.Conclude concludeReq -> do
      expRollout <- LYSQADLR.findByPrimaryKey concludeReq.domain merchantOpCityId "Unbounded" concludeReq.version >>= fromMaybeM (InvalidRequest $ "Rollout not found for Domain: " <> show concludeReq.domain <> " City: " <> show merchantOpCityId <> " TimeBounds: " <> "Unbounded" <> " Version: " <> show concludeReq.version)
      let concludedRollout =
            expRollout
              { experimentStatus = Just Lib.Yudhishthira.Types.CONCLUDED,
                isBaseVersion = Just True,
                percentageRollout = expRollout.percentageRollout + fromMaybe 0 originalBasePercentage
              }
      LYSQADLR.updateByPrimaryKey concludedRollout
      CADLR.clearDomainCache (cast merchantOpCityId) concludeReq.domain
      CADLR.clearCityConfigsCache (cast merchantOpCityId)
      fork "push concluded_rollout to kafka" $ pushToKafka concludedRollout "concluded-rollout" ""
      pure Kernel.Types.APISuccess.Success
    Lib.Yudhishthira.Types.Abort abortReq -> do
      expRollout <- LYSQADLR.findByPrimaryKey abortReq.domain merchantOpCityId "Unbounded" abortReq.version >>= fromMaybeM (InvalidRequest $ "Rollout not found for Domain: " <> show abortReq.domain <> " City: " <> show merchantOpCityId <> " TimeBounds: " <> "Unbounded" <> " Version: " <> show abortReq.version)
      mbBaseRollout <- LYSQADLR.findByCityAndDomainAndIsBase merchantOpCityId abortReq.domain
      when (isNothing mbBaseRollout) $ throwError $ InvalidRequest $ "No Base version rollout found for " <> show abortReq.domain
      let baseRollout = fromJust mbBaseRollout
          updatedBaseRollout =
            baseRollout
              { percentageRollout = baseRollout.percentageRollout + expRollout.percentageRollout
              }
          abortedRollout =
            expRollout
              { percentageRollout = 0,
                experimentStatus = Just Lib.Yudhishthira.Types.DISCARDED
              }
      when (abortReq.version == baseRollout.version) $ throwError $ InvalidRequest "Cannot abort the base rollout"
      LYSQADLR.updateByPrimaryKey abortedRollout
      LYSQADLR.updateByPrimaryKey updatedBaseRollout
      CADLR.clearDomainCache (cast merchantOpCityId) abortReq.domain
      CADLR.clearCityConfigsCache (cast merchantOpCityId)
      fork "push aborted_rollout to kafka" $ pushToKafka abortedRollout "aborted-rollout" ""
      pure Kernel.Types.APISuccess.Success
    Lib.Yudhishthira.Types.Revert revertReq -> do
      mbBaseRollout <- LYSQADLR.findByCityAndDomainAndIsBase merchantOpCityId revertReq.domain
      when (isNothing mbBaseRollout) $ throwError $ InvalidRequest $ "No Base version rollout found for " <> show revertReq.domain
      now <- getCurrentTime
      newVersion <- do
        latestElement <- QADLE.findLatestVersion (Just 1) Nothing revertReq.domain
        return (maybe 1 ((+ 1) . (.version)) (listToMaybe latestElement))
      let baseRollout = fromJust mbBaseRollout
          newBaseElement = mkBaseAppDynamicLogicElement newVersion baseElementPatch 0 now
          newBaseRollout = mkBaseAppDynamicLogicRollout newVersion baseRollout now
          revertedBaseRollout =
            baseRollout
              { isBaseVersion = Nothing,
                percentageRollout = 0,
                experimentStatus = Just Lib.Yudhishthira.Types.REVERTED
              }
      LYSQADLR.updateByPrimaryKey revertedBaseRollout
      LYSQADLE.create newBaseElement
      LYSQADLR.create newBaseRollout
      CADLE.clearCache revertReq.domain
      CADLR.clearDomainCache (cast merchantOpCityId) revertReq.domain
      CADLR.clearCityConfigsCache (cast merchantOpCityId)
      pure Kernel.Types.APISuccess.Success
      where
        mkBaseAppDynamicLogicElement :: Int -> A.Value -> Int -> UTCTime -> DTADLE.AppDynamicLogicElement
        mkBaseAppDynamicLogicElement version logic order now =
          DTADLE.AppDynamicLogicElement
            { createdAt = now,
              updatedAt = now,
              merchantId = mbMerchantId,
              domain = revertReq.domain,
              description = Nothing,
              ..
            }
        mkBaseAppDynamicLogicRollout :: Int -> AppDynamicLogicRollout -> UTCTime -> AppDynamicLogicRollout
        mkBaseAppDynamicLogicRollout newVersion originalBaseRollout now =
          AppDynamicLogicRollout
            { domain = revertReq.domain,
              experimentStatus = Just Lib.Yudhishthira.Types.RUNNING,
              isBaseVersion = Just True,
              merchantId = mbMerchantId,
              merchantOperatingCityId = merchantOpCityId,
              modifiedBy = Nothing,
              percentageRollout = originalBaseRollout.percentageRollout,
              timeBounds = "Unbounded",
              version = newVersion,
              versionDescription = Nothing,
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
