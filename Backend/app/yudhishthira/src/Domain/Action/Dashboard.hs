{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard where

import qualified Data.List as DL
import Data.OpenApi (ToSchema)
import qualified Domain.Types.ChakraQueries
import qualified Domain.Types.NammaTag as DNT
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types
import Servant hiding (throwError)
import qualified Storage.Queries.ChakraQueries as SQCQ
import qualified Storage.Queries.NammaTag as QNT
import Tools.Auth
import Tools.Error
import Tools.Utils

postTagCreate :: (Verified -> Lib.Yudhishthira.Types.NammaTag -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postTagCreate _ tagRequest = do
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

postQueryCreate :: (Verified -> Domain.Types.ChakraQueries.ChakraQueriesAPIEntity -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postQueryCreate _ queryRequest = do
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
      let Domain.Types.ChakraQueries.ChakraQueriesAPIEntity {..} = queryRequest
      return $ Domain.Types.ChakraQueries.ChakraQueries {createdAt = now, updatedAt = now, ..}

    checkIfMandtoryFieldsArePresent newQueryFields = do
      let missingFields = filter (\field -> field `notElem` newQueryFields) mandatoryChakraFields
      unless (null missingFields) $ throwError (MissingQueryFields missingFields)

    checkIfFieldsAreNotRepeated newQueryFields existingQueryFields = do
      let repeatedFields = newQueryFields `DL.intersect` existingQueryFields
      unless (null repeatedFields) $ throwError (RepeatedQueryFields repeatedFields)
