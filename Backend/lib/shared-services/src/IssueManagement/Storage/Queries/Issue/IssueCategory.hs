{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.IssueCategory where

import Database.Beam.Postgres (Postgres)
import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IssueCategory
import qualified IssueManagement.Domain.Types.Issue.IssueCategory as DomainIC
import IssueManagement.Domain.Types.Issue.IssueTranslation as DomainIT
import qualified IssueManagement.Storage.Beam.Issue.IssueCategory as BeamIC
import qualified IssueManagement.Storage.Beam.Issue.IssueTranslation as BeamIT
import IssueManagement.Storage.BeamFlow
import IssueManagement.Storage.Queries.Issue.IssueTranslation ()
import IssueManagement.Tools.UtilsTH hiding (label)
import Kernel.External.Types (Language)
import Kernel.Types.Id

create :: BeamFlow m r => IssueCategory -> m ()
create = createWithKV

updateByPrimaryKey :: BeamFlow m r => IssueCategory -> m ()
updateByPrimaryKey IssueCategory {..} =
  updateWithKV
    [ Set BeamIC.category category,
      Set BeamIC.logoUrl logoUrl,
      Set BeamIC.priority priority,
      Set BeamIC.isActive isActive,
      Set BeamIC.maxAllowedRideAge maxAllowedRideAge,
      Set BeamIC.allowedRideStatuses allowedRideStatuses,
      Set BeamIC.label label,
      Set BeamIC.igmCategory igmCategory,
      Set BeamIC.createdAt createdAt,
      Set BeamIC.updatedAt updatedAt
    ]
    [Is BeamIC.id $ Eq (getId id)]

findAllIssueTranslationWithSeCondition :: BeamFlow m r => [Clause Postgres BeamIT.IssueTranslationT] -> m [IssueTranslation]
findAllIssueTranslationWithSeCondition = findAllWithKV

findAllIssueCategoryWithSeCondition :: BeamFlow m r => [Clause Postgres BeamIC.IssueCategoryT] -> OrderBy BeamIC.IssueCategoryT -> Maybe Int -> Maybe Int -> m [IssueCategory]
findAllIssueCategoryWithSeCondition = findAllWithOptionsKV

findAllActiveByMerchantOpCityIdAndLanguage :: BeamFlow m r => Id MerchantOperatingCity -> Language -> m [(IssueCategory, Maybe IssueTranslation)]
findAllActiveByMerchantOpCityIdAndLanguage merchantOpCityId language = do
  iTranslations <- findAllIssueTranslationWithSeCondition [Is BeamIT.language $ Eq language]
  iCategorys <- findAllIssueCategoryWithSeCondition [And [Is BeamIC.isActive $ Eq True, Is BeamIC.merchantOperatingCityId $ Eq (getId merchantOpCityId)]] (Asc BeamIC.priority) Nothing Nothing
  pure $ foldl' (getIssueCategoryWithTranslations iTranslations) [] iCategorys
  where
    getIssueCategoryWithTranslations iTranslations dInfosWithTranslations iCategory =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iCategory.category) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iCategory, Just iTranslation'')) <$> iTranslations' else [(iCategory, Nothing)]

findById :: BeamFlow m r => Id IssueCategory -> m (Maybe IssueCategory)
findById (Id issueCategoryId) = findOneWithKV [Is BeamIC.id $ Eq issueCategoryId]

findByIdAndLanguage :: BeamFlow m r => Id IssueCategory -> Language -> m (Maybe (IssueCategory, Maybe IssueTranslation))
findByIdAndLanguage (Id issueCategoryId) language = do
  iCategory <- findAllIssueCategoryWithSeCondition [Is BeamIC.id $ Eq issueCategoryId] (Asc BeamIC.priority) Nothing Nothing
  iTranslations <- findAllIssueTranslationWithSeCondition [And [Is BeamIT.language $ Eq language, Is BeamIT.sentence $ In (DomainIC.category <$> iCategory)]]
  let dInfosWithTranslations' = foldl' (getIssueOptionsWithTranslations iTranslations) [] iCategory
      dInfosWithTranslations = headMaybe dInfosWithTranslations'
  pure dInfosWithTranslations
  where
    getIssueOptionsWithTranslations iTranslations dInfosWithTranslations iCategory =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iCategory.category) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iCategory, Just iTranslation'')) <$> iTranslations' else [(iCategory, Nothing)]

    headMaybe dInfosWithTranslations' = if null dInfosWithTranslations' then Nothing else Just (head dInfosWithTranslations')

findCategoriesByMinPriority :: BeamFlow m r => Int -> m [IssueCategory]
findCategoriesByMinPriority priority =
  findAllWithKV [Is BeamIC.priority $ GreaterThanOrEq priority]

updatePriority :: BeamFlow m r => Id IssueCategory -> Int -> m ()
updatePriority issueCategoryId priority =
  updateWithKV
    [Set BeamIC.priority priority]
    [Is BeamIC.id $ Eq (getId issueCategoryId)]

updateIsActive :: BeamFlow m r => Id IssueCategory -> Bool -> m ()
updateIsActive issueCategoryId isActive = do
  now <- getCurrentTime
  updateWithKV
    [ Set BeamIC.isActive isActive,
      Set BeamIC.updatedAt now
    ]
    [Is BeamIC.id $ Eq (getId issueCategoryId)]

findByIGMIssueCategory :: BeamFlow m r => Text -> m (Maybe IssueCategory)
findByIGMIssueCategory igmCategory = findOneWithKV [Is BeamIC.igmCategory $ Eq $ Just igmCategory]

findByNameAndMerchantOpCityId :: BeamFlow m r => Text -> Id MerchantOperatingCity -> m (Maybe IssueCategory)
findByNameAndMerchantOpCityId categoryName merchantOpCityId = findOneWithKV [And [Is BeamIC.category $ Eq categoryName, Is BeamIC.merchantOperatingCityId $ Eq (getId merchantOpCityId)]]

instance FromTType' BeamIC.IssueCategory IssueCategory where
  fromTType' BeamIC.IssueCategoryT {..} = do
    pure $
      Just
        IssueCategory
          { id = Id id,
            merchantId = Id merchantId,
            merchantOperatingCityId = Id merchantOperatingCityId,
            ..
          }

instance ToTType' BeamIC.IssueCategory IssueCategory where
  toTType' IssueCategory {..} = do
    BeamIC.IssueCategoryT
      { BeamIC.id = getId id,
        BeamIC.category = category,
        BeamIC.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamIC.logoUrl = logoUrl,
        BeamIC.priority = priority,
        BeamIC.merchantId = getId merchantId,
        BeamIC.categoryType = categoryType,
        BeamIC.isRideRequired = isRideRequired,
        BeamIC.isTicketRequired = isTicketRequired,
        BeamIC.maxAllowedRideAge = maxAllowedRideAge,
        BeamIC.allowedRideStatuses = allowedRideStatuses,
        BeamIC.label = label,
        BeamIC.isActive = isActive,
        BeamIC.createdAt = createdAt,
        BeamIC.updatedAt = updatedAt,
        BeamIC.igmCategory = igmCategory
      }
