{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Person (module Storage.Queries.Person, module ReExport) where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Notification.FCM.Types
import qualified Kernel.External.Whatsapp.Interface.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Lib.Yudhishthira.Tools.Utils
import qualified Lib.Yudhishthira.Types
import qualified Sequelize as Se
import qualified Storage.Beam.Person as Beam
import Storage.Queries.PersonExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Person.Person -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Person.Person] -> m ())
createMany = traverse_ create

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByMerchantId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> [Domain.Types.Person.Role] -> m [Domain.Types.Person.Person])
findAllByMerchantId merchantId role = do findAllWithDb [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId), Se.Is Beam.role $ Se.In role]]

findByEmailAndMerchant ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.Person.Person))
findByEmailAndMerchant email merchantId = do findOneWithKV [Se.And [Se.Is Beam.email $ Se.Eq email, Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.Person.Person))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByIdAndRoleAndMerchantId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.Person.Role -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.Person.Person))
findByIdAndRoleAndMerchantId id role merchantId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id),
          Se.Is Beam.role $ Se.Eq role,
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)
        ]
    ]

findByIdentifierAndMerchant ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.Person.Person))
findByIdentifierAndMerchant identifier merchantId = do findOneWithKV [Se.And [Se.Is Beam.identifier $ Se.Eq identifier, Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]]

setIsNewFalse :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
setIsNewFalse isNew id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.isNew isNew, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateClientId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateClientId clientId id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.clientId clientId, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateDeviceToken ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.External.Notification.FCM.Types.FCMRecipientToken -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateDeviceToken deviceToken id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.deviceToken deviceToken, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateDriverTag :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe [Lib.Yudhishthira.Types.TagNameValueExpiry] -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateDriverTag driverTag id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.driverTag (Lib.Yudhishthira.Tools.Utils.tagsNameValueExpiryToTType driverTag), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateName :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateName firstName id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.firstName firstName, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateTotalEarnedCoins :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateTotalEarnedCoins totalEarnedCoins id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.totalEarnedCoins totalEarnedCoins, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateUsedCoins :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateUsedCoins usedCoins id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.usedCoins usedCoins, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateWhatsappNotificationEnrollStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.External.Whatsapp.Interface.Types.OptApiMethods -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateWhatsappNotificationEnrollStatus whatsappNotificationEnrollStatus id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.whatsappNotificationEnrollStatus whatsappNotificationEnrollStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.Person.Person))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Person.Person -> m ())
updateByPrimaryKey (Domain.Types.Person.Person {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.alternateMobileNumberEncrypted (alternateMobileNumber <&> unEncrypted . (.encrypted)),
      Se.Set Beam.alternateMobileNumberHash (alternateMobileNumber <&> (.hash)),
      Se.Set Beam.backendAppVersion backendAppVersion,
      Se.Set Beam.backendConfigVersion (fmap Kernel.Utils.Version.versionToText backendConfigVersion),
      Se.Set Beam.clientBundleVersion (fmap Kernel.Utils.Version.versionToText clientBundleVersion),
      Se.Set Beam.clientConfigVersion (fmap Kernel.Utils.Version.versionToText clientConfigVersion),
      Se.Set Beam.clientManufacturer (clientDevice >>= (.deviceManufacturer)),
      Se.Set Beam.clientModelName (clientDevice <&> (.deviceModel)),
      Se.Set Beam.clientOsType (clientDevice <&> (.deviceType)),
      Se.Set Beam.clientOsVersion (clientDevice <&> (.deviceVersion)),
      Se.Set Beam.clientId clientId,
      Se.Set Beam.clientSdkVersion (fmap Kernel.Utils.Version.versionToText clientSdkVersion),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.description description,
      Se.Set Beam.deviceToken deviceToken,
      Se.Set Beam.driverTag (Lib.Yudhishthira.Tools.Utils.tagsNameValueExpiryToTType driverTag),
      Se.Set Beam.email email,
      Se.Set Beam.faceImageId (Kernel.Types.Id.getId <$> faceImageId),
      Se.Set Beam.firstName firstName,
      Se.Set Beam.gender gender,
      Se.Set Beam.hometown hometown,
      Se.Set Beam.identifier identifier,
      Se.Set Beam.identifierType identifierType,
      Se.Set Beam.isNew isNew,
      Se.Set Beam.language language,
      Se.Set Beam.languagesSpoken languagesSpoken,
      Se.Set Beam.lastName lastName,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Just $ Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.middleName middleName,
      Se.Set Beam.mobileCountryCode mobileCountryCode,
      Se.Set Beam.mobileNumberEncrypted (mobileNumber <&> unEncrypted . (.encrypted)),
      Se.Set Beam.mobileNumberHash (mobileNumber <&> (.hash)),
      Se.Set Beam.onboardedFromDashboard onboardedFromDashboard,
      Se.Set Beam.passwordHash passwordHash,
      Se.Set Beam.registrationLat registrationLat,
      Se.Set Beam.registrationLon registrationLon,
      Se.Set Beam.role role,
      Se.Set Beam.totalEarnedCoins totalEarnedCoins,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.useFakeOtp useFakeOtp,
      Se.Set Beam.usedCoins usedCoins,
      Se.Set Beam.whatsappNotificationEnrollStatus whatsappNotificationEnrollStatus
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
