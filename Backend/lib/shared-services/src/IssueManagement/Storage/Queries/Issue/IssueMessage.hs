{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.IssueMessage where

import Database.Beam.Postgres (Postgres)
import qualified IssueManagement.Domain.Types.Issue.IssueCategory as DIC
import IssueManagement.Domain.Types.Issue.IssueMessage as DomainIM
import qualified IssueManagement.Domain.Types.Issue.IssueOption as DIO
import IssueManagement.Domain.Types.Issue.IssueTranslation
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import qualified IssueManagement.Storage.Beam.Issue.IssueMessage as BeamIM
import qualified IssueManagement.Storage.Beam.Issue.IssueTranslation as BeamIT
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.Queries.Issue.IssueTranslation ()
import IssueManagement.Tools.UtilsTH hiding (label)
import Kernel.External.Types (Language)
import Kernel.Types.Id

create :: BeamFlow m r => IssueMessage -> m ()
create = createWithKV

updateByPrimaryKey :: BeamFlow m r => IssueMessage -> m ()
updateByPrimaryKey IssueMessage {..} =
  updateWithKV
    [ Set BeamIM.categoryId (getId <$> categoryId),
      Set BeamIM.optionId (getId <$> optionId),
      Set BeamIM.merchantOperatingCityId (getId merchantOperatingCityId),
      Set BeamIM.priority priority,
      Set BeamIM.message message,
      Set BeamIM.messageTitle messageTitle,
      Set BeamIM.messageAction messageAction,
      Set BeamIM.referenceCategoryId (getId <$> referenceCategoryId),
      Set BeamIM.referenceOptionId (getId <$> referenceOptionId),
      Set BeamIM.label label,
      Set BeamIM.isActive isActive,
      Set BeamIM.mediaFiles (getId <$> mediaFiles),
      Set BeamIM.createdAt createdAt,
      Set BeamIM.updatedAt updatedAt
    ]
    [And [Is BeamIM.id $ Eq (getId id)]]

findAllIssueTranslationWithSeCondition :: BeamFlow m r => [Clause Postgres BeamIT.IssueTranslationT] -> m [IssueTranslation]
findAllIssueTranslationWithSeCondition = findAllWithKV

findAllIssueMessageWithSeCondition :: BeamFlow m r => [Clause Postgres BeamIM.IssueMessageT] -> OrderBy BeamIM.IssueMessageT -> Maybe Int -> Maybe Int -> m [IssueMessage]
findAllIssueMessageWithSeCondition = findAllWithOptionsKV

findById :: BeamFlow m r => Id IssueMessage -> m (Maybe IssueMessage)
findById issueMessageId = findOneWithKV [Is BeamIM.id $ Eq (getId issueMessageId)]

findByIdAndLanguage :: BeamFlow m r => Id IssueMessage -> Language -> m (Maybe (IssueMessage, DetailedTranslation))
findByIdAndLanguage issueMessageId language = do
  iMessages <- findAllWithKV [Is BeamIM.id $ Eq (getId issueMessageId)]
  iTranslations <- findAllIssueTranslationWithSeCondition $ translationClause iMessages language
  pure $ headMaybe $ foldl' (getIssueMessagesWithTranslations iTranslations) [] iMessages
  where
    getIssueMessagesWithTranslations iTranslations dInfosWithTranslations iMessage =
      let detailedTranslation = mkDetailedTranslation iTranslations iMessage
       in dInfosWithTranslations <> [(iMessage, detailedTranslation)]
    headMaybe dInfosWithTranslations' = if null dInfosWithTranslations' then Nothing else Just (head dInfosWithTranslations')

findAllActiveByCategoryIdAndLanguage :: BeamFlow m r => Id DIC.IssueCategory -> Language -> m [(IssueMessage, DetailedTranslation)]
findAllActiveByCategoryIdAndLanguage (Id issueCategoryId) language = do
  iMessages <- findAllIssueMessageWithSeCondition [And [Is BeamIM.categoryId $ Eq $ Just issueCategoryId, Is BeamIM.isActive $ Eq True]] (Asc BeamIM.priority) Nothing Nothing
  iTranslations <- findAllIssueTranslationWithSeCondition $ translationClause iMessages language
  pure $ foldl' (getIssueMessagesWithTranslations iTranslations) [] iMessages
  where
    getIssueMessagesWithTranslations iTranslations dInfosWithTranslations iMessage =
      let detailedTranslation = mkDetailedTranslation iTranslations iMessage
       in dInfosWithTranslations <> [(iMessage, detailedTranslation)]

findAllActiveByOptionIdAndLanguage :: BeamFlow m r => Id DIO.IssueOption -> Language -> m [(IssueMessage, DetailedTranslation)]
findAllActiveByOptionIdAndLanguage (Id issueOptionId) language = do
  iMessages <- findAllIssueMessageWithSeCondition [And [Is BeamIM.optionId $ Eq $ Just issueOptionId, Is BeamIM.isActive $ Eq True]] (Asc BeamIM.priority) Nothing Nothing
  iTranslations <- findAllIssueTranslationWithSeCondition $ translationClause iMessages language
  pure $ foldl' (getIssueMessagesWithTranslations iTranslations) [] iMessages
  where
    getIssueMessagesWithTranslations iTranslations dInfosWithTranslations iMessage =
      let detailedTranslation = mkDetailedTranslation iTranslations iMessage
       in dInfosWithTranslations <> [(iMessage, detailedTranslation)]

translationClause :: [IssueMessage] -> Language -> [Clause Postgres BeamIT.IssueTranslationT]
translationClause iMessages language =
  [ And
      [ Is BeamIT.language $ Eq language,
        Or
          [ Is BeamIT.sentence $ In (DomainIM.message <$> iMessages),
            Is BeamIT.sentence $ In $ catMaybes (DomainIM.messageTitle <$> iMessages),
            Is BeamIT.sentence $ In $ mapMaybe DomainIM.messageAction iMessages
          ]
      ]
  ]

findAllWithMinPriority :: BeamFlow m r => Int -> m [IssueMessage]
findAllWithMinPriority priority =
  findAllWithKV [Is BeamIM.priority $ GreaterThanOrEq priority]

updatePriority :: BeamFlow m r => Id IssueMessage -> Int -> m ()
updatePriority issueOptionId priority =
  updateWithKV
    [ Set BeamIM.priority priority
    ]
    [Is BeamIM.id $ Eq (getId issueOptionId)]

updateMediaFiles :: BeamFlow m r => Id IssueMessage -> [Id DMF.MediaFile] -> m ()
updateMediaFiles issueMessageId mediaFiles = do
  now <- getCurrentTime
  updateWithKV
    [ Set BeamIM.mediaFiles $ getId <$> mediaFiles,
      Set BeamIM.updatedAt now
    ]
    [Is BeamIM.id $ Eq $ getId issueMessageId]

updateIsActive :: BeamFlow m r => Id IssueMessage -> Bool -> m ()
updateIsActive issueMessageId isActive = do
  now <- getCurrentTime
  updateWithKV
    [ Set BeamIM.isActive isActive,
      Set BeamIM.updatedAt now
    ]
    [Is BeamIM.id $ Eq (getId issueMessageId)]

mkDetailedTranslation :: [IssueTranslation] -> IssueMessage -> DetailedTranslation
mkDetailedTranslation iTranslations issueMessage =
  DetailedTranslation
    { titleTranslation = findTranslation issueMessage.messageTitle,
      contentTranslation = findTranslation (Just issueMessage.message),
      actionTranslation = findTranslation issueMessage.messageAction
    }
  where
    findTranslation :: Maybe Text -> Maybe IssueTranslation
    findTranslation Nothing = Nothing
    findTranslation (Just msg) = find (\iTranslation -> iTranslation.sentence == msg) iTranslations

instance FromTType' BeamIM.IssueMessage IssueMessage where
  fromTType' BeamIM.IssueMessageT {..} = do
    pure $
      Just
        IssueMessage
          { id = Id id,
            categoryId = Id <$> categoryId,
            optionId = Id <$> optionId,
            merchantId = Id merchantId,
            merchantOperatingCityId = Id merchantOperatingCityId,
            referenceCategoryId = Id <$> referenceCategoryId,
            referenceOptionId = Id <$> referenceOptionId,
            mediaFiles = Id <$> mediaFiles,
            ..
          }

instance ToTType' BeamIM.IssueMessage IssueMessage where
  toTType' IssueMessage {..} = do
    BeamIM.IssueMessageT
      { BeamIM.id = getId id,
        BeamIM.categoryId = getId <$> categoryId,
        BeamIM.optionId = getId <$> optionId,
        BeamIM.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamIM.referenceCategoryId = getId <$> referenceCategoryId,
        BeamIM.referenceOptionId = getId <$> referenceOptionId,
        BeamIM.mediaFiles = getId <$> mediaFiles,
        BeamIM.message = message,
        BeamIM.messageTitle = messageTitle,
        BeamIM.messageAction = messageAction,
        BeamIM.messageType = messageType,
        BeamIM.isActive = isActive,
        BeamIM.priority = priority,
        BeamIM.merchantId = getId merchantId,
        BeamIM.label = label,
        BeamIM.createdAt = createdAt,
        BeamIM.updatedAt = updatedAt
      }
