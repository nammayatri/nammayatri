{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Flow.Dashboard where

import qualified Data.Aeson as A
import qualified Data.List as DL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.Queries.ChakraQueries as SQCQ
import qualified Lib.Yudhishthira.Storage.Queries.NammaTag as QNT
import Lib.Yudhishthira.Tools.Error
import Lib.Yudhishthira.Tools.Utils
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.ChakraQueries
import qualified Lib.Yudhishthira.Types.NammaTag as DNT
import Servant hiding (throwError)

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

postQueryCreate :: BeamFlow m r => Lib.Yudhishthira.Types.ChakraQueriesAPIEntity -> m Kernel.Types.APISuccess.APISuccess
postQueryCreate queryRequest = do
  existingQueryFields <- getChakraQueryFields queryRequest.chakra
  checkIfMandtoryFieldsArePresent queryRequest.queryResults
  checkIfFieldsAreNotRepeated queryRequest.queryResults existingQueryFields
  query <- buildQuery
  SQCQ.create query
  pure Kernel.Types.APISuccess.Success
  where
    buildQuery = do
      now <- getCurrentTime
      id <- generateGUID
      let Lib.Yudhishthira.Types.ChakraQueriesAPIEntity {..} = queryRequest
      return $ Lib.Yudhishthira.Types.ChakraQueries.ChakraQueries {createdAt = now, updatedAt = now, ..}

    checkIfMandtoryFieldsArePresent newQueryFields = do
      let missingFields = filter (\field -> field `notElem` newQueryFields) mandatoryChakraFields
      unless (null missingFields) $ throwError (MissingQueryFields missingFields)

    checkIfFieldsAreNotRepeated newQueryFields existingQueryFields = do
      let repeatedFields = newQueryFields `DL.intersect` existingQueryFields
      unless (null repeatedFields) $ throwError (RepeatedQueryFields repeatedFields)

verifyDynamicLogic :: (BeamFlow m r, ToJSON a) => [Value] -> a -> m Lib.Yudhishthira.Types.AppDynamicLogicResp
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
    _ -> throwError $ InvalidRequest "Tag should have format of name#value"
