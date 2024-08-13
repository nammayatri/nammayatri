{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Flow.Dashboard where

import qualified Data.List as DL
import Data.OpenApi (ToSchema)
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
