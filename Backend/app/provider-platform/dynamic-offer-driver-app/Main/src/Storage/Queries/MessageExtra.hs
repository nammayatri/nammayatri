{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MessageExtra where

import qualified Data.Time as T
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Message
import Domain.Types.MessageTranslation as DomainMT
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Message as BeamM
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.MessageTranslation as MT
import Storage.Queries.OrphanInstances.Message
import Tools.Error

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
  findById messageId >>= \case
    Nothing -> pure ()
    Just msg -> do
      let likeCount = msg.likeCount
      updateOneWithKV
        [Se.Set BeamM.likeCount $ likeCount + value]
        [Se.Is BeamM.id (Se.Eq $ getId messageId)]

updateMessageViewCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Message -> Int -> m ()
updateMessageViewCount messageId value = do
  findById messageId >>= \case
    Just msg -> do
      let viewCount = msg.viewCount
      updateOneWithKV
        [Se.Set BeamM.viewCount $ viewCount + value]
        [Se.Is BeamM.id (Se.Eq $ getId messageId)]
    Nothing -> pure ()

castToRawMessage :: Message -> RawMessage
castToRawMessage Message {..} = RawMessage {..}
