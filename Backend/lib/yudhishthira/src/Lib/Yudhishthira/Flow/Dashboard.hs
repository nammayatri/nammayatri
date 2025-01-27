module Lib.Yudhishthira.Flow.Dashboard where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import Data.List (nub, sortBy)
import qualified Data.List.NonEmpty as DLNE
import Data.Scientific (toRealFloat)
import qualified Data.Text as T
import JsonLogic
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
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicElement as QADLE
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
                createdAt = now,
                updatedAt = now
              }
        Lib.Yudhishthira.Types.KaalChakraTag Lib.Yudhishthira.Types.NammaTagChakra {..} ->
          return $
            DNT.NammaTag
              { category = tagCategory,
                info = DNT.KaalChakra (DNT.KaalChakraTagInfo tagChakra tagValidity),
                name = tagName,
                possibleValues = tagPossibleValues,
                rule = tagRule,
                description = description,
                actionEngine,
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
      return $
        DNT.NammaTag
          { category = fromMaybe tag.category tagRequest.tagCategory,
            info = buildTagInfo tag,
            name = tag.name,
            possibleValues = fromMaybe tag.possibleValues tagRequest.tagPossibleValues,
            rule = fromMaybe tag.rule tagRequest.tagRule,
            description = tagRequest.description <|> tag.description,
            actionEngine = tagRequest.actionEngine <|> tag.actionEngine,
            createdAt = tag.createdAt,
            updatedAt = now
          }
    buildTagInfo tag = do
      case tag.info of
        DNT.Application (DNT.ApplicationTagInfo tagStage) -> DNT.Application (DNT.ApplicationTagInfo (fromMaybe tagStage tagRequest.tagStage))
        DNT.KaalChakra (DNT.KaalChakraTagInfo tagChakra tagValidity) -> DNT.KaalChakra (DNT.KaalChakraTagInfo (fromMaybe tagChakra tagRequest.tagChakra) (tagRequest.tagValidity <|> tagValidity))
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
  checkIfMandatoryFieldsArePresent queryRequest.queryResults
  checkIfFieldsAreNotRepeated (queryRequest.queryResults <&> (.resultName))
  existingChakraQuery <- SQCQ.findByPrimaryKey queryRequest.chakra queryRequest.queryName
  whenJust existingChakraQuery $ \_ -> do
    throwError $ InvalidRequest "Chakra query with this name already exists"
  chakraQuery <- buildQuery

  -- TODO find a better way to validate query than run it
  let template =
        KaalChakraEvent.Template
          { limit = Lib.Yudhishthira.Types.QLimit 10,
            offset = Lib.Yudhishthira.Types.QOffset 0,
            usersSet = Lib.Yudhishthira.Types.ALL_USERS
          }
  void $ KaalChakraEvent.runQueryRequestTemplate chakraQuery template
  SQCQ.create chakraQuery

  pure Kernel.Types.APISuccess.Success
  where
    buildQuery = do
      now <- getCurrentTime
      let Lib.Yudhishthira.Types.ChakraQueriesAPIEntity {..} = queryRequest
      return $ Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries {createdAt = now, updatedAt = now, ..}

    checkIfMandatoryFieldsArePresent newQueryFields = do
      let missingFields = filter (\field -> field `notElem` (newQueryFields <&> (.resultName))) mandatoryChakraFields
      unless (null missingFields) $ throwError (MissingQueryFields missingFields)

    checkIfFieldsAreNotRepeated newQueryFieldNames = do
      let repeatedFields = filter (\f -> length (filter (== f) newQueryFieldNames) > 1) newQueryFieldNames
      unless (null repeatedFields) $ throwError (RepeatedQueryFields repeatedFields)

verifyTag :: BeamFlow m r => Text -> m ()
verifyTag fullTag = do
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
    [_] -> return ()
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
  (BeamFlow m r, ToJSON a, FromJSON b) =>
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
      latestElement <- QADLE.findLatestVersion (Just 1) Nothing domain
      let version = maybe 1 ((+ 1) . (.version)) (listToMaybe latestElement)
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
       in Just $ Lib.Yudhishthira.Types.LogicRolloutObject firstElement.domain firstElement.timeBounds (DLNE.toList rollout)

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
  rolloutObjectsArr <- mapM (mkAppDynamicLogicRolloutDomain merchantOpCityId now) rolloutReq
  let rolloutObjects = concat rolloutObjectsArr
  CADLR.delete (cast merchantOpCityId) domain
  CADLR.createMany rolloutObjects
  CADLR.clearCache (cast merchantOpCityId) domain
  return Kernel.Types.APISuccess.Success
  where
    getDomain :: Maybe Lib.Yudhishthira.Types.LogicDomain
    getDomain = listToMaybe $ map (.domain) rolloutReq

    mkAppDynamicLogicRolloutDomain :: BeamFlow m r => Id Lib.Yudhishthira.Types.MerchantOperatingCity -> UTCTime -> Lib.Yudhishthira.Types.LogicRolloutObject -> m [AppDynamicLogicRollout]
    mkAppDynamicLogicRolloutDomain merchantOperatingCityId now Lib.Yudhishthira.Types.LogicRolloutObject {..} = do
      when (timeBounds /= "Unbounded") $
        void $ CQTBC.findByPrimaryKey merchantOperatingCityId timeBounds domain >>= fromMaybeM (InvalidRequest $ "Time bound not found: " <> timeBounds)
      let rolloutSum = sum $ map (.rolloutPercentage) rollout
      when (rolloutSum /= 100) $ throwError $ InvalidRequest "Sum of rollout percentage should be 100"
      mapM (mkAppDynamicLogicRollout merchantOperatingCityId now domain timeBounds) rollout

    mkAppDynamicLogicRollout ::
      BeamFlow m r =>
      Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
      UTCTime ->
      Lib.Yudhishthira.Types.LogicDomain ->
      Text ->
      Lib.Yudhishthira.Types.RolloutVersion ->
      m AppDynamicLogicRollout
    mkAppDynamicLogicRollout merchantOperatingCityId now domain timeBounds Lib.Yudhishthira.Types.RolloutVersion {..} = do
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

isString :: A.Value -> Bool
isString (A.String _) = True
isString _ = False

inRange :: A.Value -> (Double, Double) -> Bool
inRange (A.Number num) (rangeStart, rangeEnd) = (toRealFloat num) >= rangeStart && (toRealFloat num) <= rangeEnd
inRange _ _ = False
