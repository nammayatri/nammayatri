{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Overlay where

import qualified Data.Aeson
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Overlay
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Overlay as Beam
import qualified Storage.Queries.Transformers.Overlay

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Overlay.Overlay -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Overlay.Overlay] -> m ())
createMany = traverse_ create

deleteByOverlayKeyMerchantOpCityIdUdf ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ())
deleteByOverlayKeyMerchantOpCityIdUdf (Kernel.Types.Id.Id merchantOperatingCityId) overlayKey udf1 = do
  deleteWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.overlayKey $ Se.Eq overlayKey,
          Se.Is Beam.udf1 $ Se.Eq udf1
        ]
    ]

findAllByLanguage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.External.Types.Language -> m [Domain.Types.Overlay.Overlay])
findAllByLanguage (Kernel.Types.Id.Id merchantOperatingCityId) language = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.language $ Se.Eq language
        ]
    ]

findAllByMerchantOpCityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.Overlay.Overlay])
findAllByMerchantOpCityId (Kernel.Types.Id.Id merchantOperatingCityId) = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

findAllByOverlayKeyUdf ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m [Domain.Types.Overlay.Overlay])
findAllByOverlayKeyUdf (Kernel.Types.Id.Id merchantOperatingCityId) overlayKey udf1 = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.overlayKey $ Se.Eq overlayKey,
          Se.Is Beam.udf1 $ Se.Eq udf1
        ]
    ]

findByMerchantOpCityIdPNKeyLangaugeUdf ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Text -> Kernel.External.Types.Language -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.Overlay.Overlay))
findByMerchantOpCityIdPNKeyLangaugeUdf (Kernel.Types.Id.Id merchantOperatingCityId) overlayKey language udf1 = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.overlayKey $ Se.Eq overlayKey,
          Se.Is Beam.language $ Se.Eq language,
          Se.Is Beam.udf1 $ Se.Eq udf1
        ]
    ]

instance FromTType' Beam.Overlay Domain.Types.Overlay.Overlay where
  fromTType' (Beam.OverlayT {..}) = do
    pure $
      Just
        Domain.Types.Overlay.Overlay
          { actions = actions,
            actions2 = fromMaybe [] $ Storage.Queries.Transformers.Overlay.valueToMaybe actions2,
            cancelButtonText = cancelButtonText,
            contactSupportNumber = contactSupportNumber,
            delay = delay,
            description = description,
            endPoint = endPoint,
            id = Kernel.Types.Id.Id id,
            imageUrl = imageUrl,
            language = language,
            link = link,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            method = method,
            okButtonText = okButtonText,
            overlayKey = overlayKey,
            reqBody = reqBody,
            secondaryActions = secondaryActions,
            secondaryActions2 = Storage.Queries.Transformers.Overlay.valueToMaybe =<< secondaryActions2,
            showPushNotification = showPushNotification,
            socialMediaLinks = Storage.Queries.Transformers.Overlay.valueToMaybe =<< socialMediaLinks,
            title = title,
            toastMessage = toastMessage,
            udf1 = udf1
          }

instance ToTType' Beam.Overlay Domain.Types.Overlay.Overlay where
  toTType' (Domain.Types.Overlay.Overlay {..}) = do
    Beam.OverlayT
      { Beam.actions = actions,
        Beam.actions2 = toJSON actions2,
        Beam.cancelButtonText = cancelButtonText,
        Beam.contactSupportNumber = contactSupportNumber,
        Beam.delay = delay,
        Beam.description = description,
        Beam.endPoint = endPoint,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.imageUrl = imageUrl,
        Beam.language = language,
        Beam.link = link,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.method = method,
        Beam.okButtonText = okButtonText,
        Beam.overlayKey = overlayKey,
        Beam.reqBody = reqBody,
        Beam.secondaryActions = secondaryActions,
        Beam.secondaryActions2 = toJSON <$> secondaryActions2,
        Beam.showPushNotification = showPushNotification,
        Beam.socialMediaLinks = Data.Aeson.toJSON <$> socialMediaLinks,
        Beam.title = title,
        Beam.toastMessage = toastMessage,
        Beam.udf1 = udf1
      }
