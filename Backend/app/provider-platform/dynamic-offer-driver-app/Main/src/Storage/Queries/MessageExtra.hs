module Storage.Queries.MessageExtra where

import API.Types.ProviderPlatform.Management.Endpoints.Message as APIT
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Message
import Domain.Types.MessageTranslation as DomainMT
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.Message as BeamM
import qualified Storage.Beam.MessageTranslation as BeamMT
import qualified Storage.Queries.MessageTranslation as MT
import Storage.Queries.OrphanInstances.Message ()

-- Extra code goes here --
createMessage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Message -> m ()
createMessage msg = do
  let mT = fmap (fn msg.id) (msg.messageTranslations)
      fn id' (Domain.Types.Message.MessageTranslation createdAt_ description_ label_ language_ shortDescription_ title_) = DomainMT.MessageTranslation createdAt_ description_ label_ language_ id' shortDescription_ title_
  MT.createMany mT >> createWithKV msg

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Message -> m ()
create = createWithKV

findAllOnboardingMessages :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Merchant -> DMOC.MerchantOperatingCity -> m [RawMessage]
findAllOnboardingMessages merchantParam moCity = do
  messages <-
    findAllWithDb
      [ Se.And
          [ Se.Is BeamM.merchantId $ Se.Eq (getId merchantParam.id),
            Se.Or
              ( [Se.Is BeamM.merchantOperatingCityId $ Se.Eq (Just $ getId $ moCity.id)]
                  <> [Se.Is BeamM.merchantOperatingCityId $ Se.Eq Nothing | merchantParam.city == moCity.city]
              ),
            Se.Is BeamM.alwaysTriggerOnOnboarding $ Se.Eq (Just True)
          ]
      ]
  pure $ map castToRawMessage messages

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Message -> m (Maybe RawMessage)
findById (Id messageId) = do
  message <- findOneWithKV [Se.Is BeamM.id $ Se.Eq messageId]
  pure $ castToRawMessage <$> message

findAllWithLimitOffset :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Int -> Maybe Int -> Merchant -> DMOC.MerchantOperatingCity -> m [RawMessage]
findAllWithLimitOffset mbLimit mbOffset merchantParam moCity = do
  messages <-
    findAllWithOptionsDb
      [ Se.And [Se.Is BeamM.merchantId $ Se.Eq (getId merchantParam.id)],
        Se.Or
          ( [Se.Is BeamM.merchantOperatingCityId $ Se.Eq (Just $ getId $ moCity.id)]
              <> [Se.Is BeamM.merchantOperatingCityId $ Se.Eq Nothing | merchantParam.city == moCity.city]
          )
      ]
      (Se.Desc BeamM.createdAt)
      (Just limitVal)
      (Just offsetVal)
  pure $ map castToRawMessage messages
  where
    limitVal = min (fromMaybe 10 mbLimit) 10
    offsetVal = fromMaybe 0 mbOffset

updateMessageLikeCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Message -> Int -> m ()
updateMessageLikeCount messageId value = do
  mbMsg <- findById messageId
  whenJust mbMsg $ \msg -> do
    let likeCount = msg.likeCount
    updateOneWithKV
      [Se.Set BeamM.likeCount $ likeCount + value]
      [Se.Is BeamM.id (Se.Eq $ getId messageId)]

updateMessageViewCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Message -> Int -> m ()
updateMessageViewCount messageId value = do
  mbMsg <- findById messageId
  whenJust mbMsg $ \msg -> do
    let viewCount = msg.viewCount
    updateOneWithKV
      [Se.Set BeamM.viewCount $ viewCount + value]
      [Se.Is BeamM.id (Se.Eq $ getId messageId)]

updateMessage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => EditMessageRequest -> m (Either Text ())
updateMessage EditMessageRequest {..} = do
  -- query for the message fields
  let updateClauses =
        catMaybes
          [ fmap (Se.Set BeamM.title) title,
            fmap (Se.Set BeamM.description) description,
            fmap (Se.Set BeamM.shortDescription) shortDescription,
            Just (Se.Set BeamM.label label)
          ]
  if null updateClauses && null messageTranslations
    then return $ Left "No fields to update"
    else do
      _ <- updateWithKV updateClauses [Se.Is BeamM.id (Se.Eq $ getId messageId)]
      -- query for message translation fields
      case messageTranslations of
        [] -> return $ Right ()
        translations -> do
          forM_ translations $ \APIT.MessageTranslation {language = newLanguage, title = newTitle, description = newDescription, shortDescription = newShortDescription, label = newLabel} -> do
            maybeTranslation <- MT.findByMessageIdAndLanguage (cast messageId) newLanguage

            case maybeTranslation of
              Just _ -> do
                let translationUpdateClauses =
                      catMaybes
                        [ Just (Se.Set BeamMT.description newDescription),
                          Just (Se.Set BeamMT.shortDescription newShortDescription),
                          Just (Se.Set BeamMT.title newTitle),
                          Just (Se.Set BeamMT.label newLabel)
                        ]
                unless (null translationUpdateClauses) $
                  updateWithKV
                    translationUpdateClauses
                    [Se.Is BeamMT.messageId (Se.Eq $ getId messageId), Se.Is BeamMT.language (Se.Eq newLanguage)]
              Nothing -> do
                now <- getCurrentTime
                let newTranslation =
                      DomainMT.MessageTranslation
                        { createdAt = now,
                          description = newDescription,
                          label = newLabel,
                          language = newLanguage,
                          messageId = cast messageId,
                          shortDescription = newShortDescription,
                          title = newTitle
                        }
                MT.createMany [newTranslation]

          return $ Right ()

castToRawMessage :: Message -> RawMessage
castToRawMessage Message {..} = RawMessage {..}
