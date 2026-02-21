{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.IssueOption where

import Database.Beam.Postgres (Postgres)
import qualified IGM.Enums as Spec
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueMessage
import IssueManagement.Domain.Types.Issue.IssueOption as DomainIO
import IssueManagement.Domain.Types.Issue.IssueTranslation
import qualified IssueManagement.Storage.Beam.Issue.IssueOption as BeamIO
import qualified IssueManagement.Storage.Beam.Issue.IssueTranslation as BeamIT
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.Queries.Issue.IssueTranslation ()
import IssueManagement.Tools.UtilsTH hiding (label)
import Kernel.External.Types (Language)
import Kernel.Types.Id

create :: BeamFlow m r => IssueOption -> m ()
create = createWithKV

updateByPrimaryKey :: BeamFlow m r => IssueOption -> m ()
updateByPrimaryKey IssueOption {..} =
  updateWithKV
    [ Set BeamIO.option option,
      Set BeamIO.issueCategoryId (getId <$> issueCategoryId),
      Set BeamIO.issueMessageId issueMessageId,
      Set BeamIO.merchantOperatingCityId (getId merchantOperatingCityId),
      Set BeamIO.priority priority,
      Set BeamIO.label label,
      Set BeamIO.isActive isActive,
      Set BeamIO.restrictedVariants restrictedVariants,
      Set BeamIO.restrictedRideStatuses restrictedRideStatuses,
      Set BeamIO.showOnlyWhenUserBlocked showOnlyWhenUserBlocked,
      Set BeamIO.igmSubCategory igmSubCategory,
      Set BeamIO.mandatoryUploads mandatoryUploads,
      Set BeamIO.createdAt createdAt,
      Set BeamIO.updatedAt updatedAt
    ]
    [And [Is BeamIO.id $ Eq (getId id)]]

findByIdAndCategoryId :: BeamFlow m r => Id IssueOption -> Id IssueCategory -> m (Maybe IssueOption)
findByIdAndCategoryId issueOptionId issueCategoryId = findOneWithKV [And [Is BeamIO.id $ Eq $ getId issueOptionId, Is BeamIO.issueCategoryId $ Eq $ Just (getId issueCategoryId)]]

findAllIssueOptionWithSeCondition :: BeamFlow m r => [Clause Postgres BeamIO.IssueOptionT] -> OrderBy BeamIO.IssueOptionT -> Maybe Int -> Maybe Int -> m [IssueOption]
findAllIssueOptionWithSeCondition = findAllWithOptionsKV

findAllIssueTranslationWithSeCondition :: BeamFlow m r => [Clause Postgres BeamIT.IssueTranslationT] -> m [IssueTranslation]
findAllIssueTranslationWithSeCondition = findAllWithKV

findAllByCategoryAndLanguage :: BeamFlow m r => Id IssueCategory -> Language -> m [(IssueOption, Maybe IssueTranslation)]
findAllByCategoryAndLanguage (Id issueCategoryId) language = do
  iOptions <- findAllIssueOptionWithSeCondition [Is BeamIO.issueCategoryId $ Eq (Just issueCategoryId)] (Asc BeamIO.priority) Nothing Nothing
  iTranslations <- findAllIssueTranslationWithSeCondition [And [Is BeamIT.language $ Eq language, Is BeamIT.sentence $ In (DomainIO.option <$> iOptions)]]
  pure $ foldl' (getIssueOptionsWithTranslations iTranslations) [] iOptions
  where
    getIssueOptionsWithTranslations iTranslations dInfosWithTranslations iOption =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iOption.option) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iOption, Just iTranslation'')) <$> iTranslations' else [(iOption, Nothing)]

findAllActiveByMessageAndLanguage :: BeamFlow m r => Id IssueMessage -> Language -> m [(IssueOption, Maybe IssueTranslation)]
findAllActiveByMessageAndLanguage (Id issueMessageId) language = do
  iOptions <- findAllIssueOptionWithSeCondition [Is BeamIO.issueMessageId $ Eq $ Just issueMessageId, Is BeamIO.isActive $ Eq True] (Asc BeamIO.priority) Nothing Nothing
  iTranslations <- findAllIssueTranslationWithSeCondition [And [Is BeamIT.language $ Eq language, Is BeamIT.sentence $ In (DomainIO.option <$> iOptions)]]
  pure $ foldl' (getIssueOptionsWithTranslations iTranslations) [] iOptions
  where
    getIssueOptionsWithTranslations iTranslations dInfosWithTranslations iOption =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iOption.option) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iOption, Just iTranslation'')) <$> iTranslations' else [(iOption, Nothing)]

findByIdAndLanguage :: BeamFlow m r => Id IssueOption -> Language -> m (Maybe (IssueOption, Maybe IssueTranslation))
findByIdAndLanguage issueOptionId language = do
  iOptions <- findAllWithKV [Is BeamIO.id $ Eq (getId issueOptionId)]
  iTranslations <- findAllIssueTranslationWithSeCondition [And [Is BeamIT.language $ Eq language, Is BeamIT.sentence $ In (DomainIO.option <$> iOptions)]]
  let dInfosWithTranslations' = foldl' (getIssueOptionsWithTranslations iTranslations) [] listToMaybe iOptions
  pure dInfosWithTranslations'
  where
    getIssueOptionsWithTranslations iTranslations dInfosWithTranslations iOption =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iOption.option) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iOption, Just iTranslation'')) <$> iTranslations' else [(iOption, Nothing)]

findById :: BeamFlow m r => Id IssueOption -> m (Maybe IssueOption)
findById (Id issueOptionId) = findOneWithKV [Is BeamIO.id $ Eq issueOptionId]

findByIGMIssueSubCategory :: BeamFlow m r => Maybe Spec.IssueSubCategory -> m (Maybe IssueOption)
findByIGMIssueSubCategory igmSubCategory = findOneWithKV [Is BeamIO.igmSubCategory $ Eq igmSubCategory]

updatePriority :: BeamFlow m r => Id IssueOption -> Int -> m ()
updatePriority issueOptionId priority =
  updateWithKV
    [ Set BeamIO.priority priority
    ]
    [Is BeamIO.id $ Eq (getId issueOptionId)]

updateIsActive :: BeamFlow m r => Id IssueOption -> Bool -> m ()
updateIsActive issueOptionId isActive = do
  now <- getCurrentTime
  updateWithKV
    [ Set BeamIO.isActive isActive,
      Set BeamIO.updatedAt now
    ]
    [Is BeamIO.id $ Eq (getId issueOptionId)]

instance FromTType' BeamIO.IssueOption IssueOption where
  fromTType' BeamIO.IssueOptionT {..} = do
    pure $
      Just
        IssueOption
          { id = Id id,
            issueCategoryId = Id <$> issueCategoryId,
            merchantId = Id merchantId,
            igmSubCategory = igmSubCategory,
            merchantOperatingCityId = Id merchantOperatingCityId,
            ..
          }

instance ToTType' BeamIO.IssueOption IssueOption where
  toTType' IssueOption {..} = do
    BeamIO.IssueOptionT
      { BeamIO.id = getId id,
        BeamIO.issueCategoryId = getId <$> issueCategoryId,
        BeamIO.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamIO.option = option,
        BeamIO.priority = priority,
        BeamIO.issueMessageId = issueMessageId,
        BeamIO.restrictedVariants = restrictedVariants,
        BeamIO.restrictedRideStatuses = restrictedRideStatuses,
        BeamIO.showOnlyWhenUserBlocked = showOnlyWhenUserBlocked,
        BeamIO.label = label,
        BeamIO.merchantId = getId merchantId,
        BeamIO.isActive = isActive,
        BeamIO.createdAt = createdAt,
        BeamIO.updatedAt = updatedAt,
        BeamIO.igmSubCategory = igmSubCategory,
        BeamIO.mandatoryUploads = mandatoryUploads
      }
