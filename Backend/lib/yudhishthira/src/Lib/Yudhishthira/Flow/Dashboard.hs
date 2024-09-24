module Lib.Yudhishthira.Flow.Dashboard where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Utils.Common
import Lib.Yudhishthira.Event.KaalChakra as KaalChakraEvent
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.Queries.ChakraQueries as SQCQ
import qualified Lib.Yudhishthira.Storage.Queries.NammaTag as QNT
import Lib.Yudhishthira.Tools.Error
import Lib.Yudhishthira.Tools.Utils
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.ChakraQueries
import qualified Lib.Yudhishthira.Types.NammaTag as DNT

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
                createdAt = now,
                updatedAt = now
              }

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

verifyDynamicLogic :: (BeamFlow m r, ToJSON a) => [Value] -> a -> m Lib.Yudhishthira.Types.RunLogicResp
verifyDynamicLogic logics data_ = runLogics logics data_

verifyTag :: BeamFlow m r => Text -> m ()
verifyTag fullTag = do
  case T.splitOn "#" fullTag of
    [name, tagValueText] -> do
      tag <- QNT.findByPrimaryKey name >>= fromMaybeM (InvalidRequest "Tag not found in the system, please create the tag")
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
  (BeamFlow m r, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) =>
  Handle m ->
  Lib.Yudhishthira.Types.RunKaalChakraJobReq ->
  m Lib.Yudhishthira.Types.RunKaalChakraJobRes
postRunKaalChakraJob h req =
  case req.usersSet of
    Lib.Yudhishthira.Types.ALL_USERS -> throwError (InvalidRequest "ALL_USERS option available only from kaal-chakra scheduler")
    _ -> kaalChakraEvent h req
