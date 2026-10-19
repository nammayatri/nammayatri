{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PersonSocialIdentity where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.PersonSocialIdentity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PersonSocialIdentity as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PersonSocialIdentity.PersonSocialIdentity -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PersonSocialIdentity.PersonSocialIdentity] -> m ())
createMany = traverse_ create

deleteAllByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteAllByPersonId personId = do deleteWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PersonSocialIdentity.PersonSocialIdentity -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

deleteByPersonIdAndProvider :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.PersonSocialIdentity.SocialLoginProvider -> m ())
deleteByPersonIdAndProvider personId provider = do deleteWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId), Se.Is Beam.provider $ Se.Eq provider]]

findAllByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.PersonSocialIdentity.PersonSocialIdentity]))
findAllByPersonId personId = do findAllWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findByMerchantIdAndProviderAndSubject ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Domain.Types.PersonSocialIdentity.SocialLoginProvider -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.PersonSocialIdentity.PersonSocialIdentity))
findByMerchantIdAndProviderAndSubject providerSubject provider merchantId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.providerSubject $ Se.Eq providerSubject,
          Se.Is Beam.provider $ Se.Eq provider,
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)
        ]
    ]

findByPersonIdAndProvider ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.PersonSocialIdentity.SocialLoginProvider -> m (Maybe Domain.Types.PersonSocialIdentity.PersonSocialIdentity))
findByPersonIdAndProvider personId provider = do findOneWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId), Se.Is Beam.provider $ Se.Eq provider]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PersonSocialIdentity.PersonSocialIdentity -> m (Maybe Domain.Types.PersonSocialIdentity.PersonSocialIdentity))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PersonSocialIdentity.PersonSocialIdentity -> m ())
updateByPrimaryKey (Domain.Types.PersonSocialIdentity.PersonSocialIdentity {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.appleRefreshToken (appleRefreshToken <&> unEncrypted),
      Se.Set Beam.clientId clientId,
      Se.Set Beam.email (email <&> unEncrypted),
      Se.Set Beam.isPrivateEmail isPrivateEmail,
      Se.Set Beam.issuer issuer,
      Se.Set Beam.lastLoginAt lastLoginAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.provider provider,
      Se.Set Beam.providerSubject providerSubject,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validTill validTill
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PersonSocialIdentity Domain.Types.PersonSocialIdentity.PersonSocialIdentity where
  fromTType' (Beam.PersonSocialIdentityT {..}) = do
    pure $
      Just
        Domain.Types.PersonSocialIdentity.PersonSocialIdentity
          { appleRefreshToken = Encrypted <$> appleRefreshToken,
            clientId = clientId,
            createdAt = createdAt,
            email = Encrypted <$> email,
            id = Kernel.Types.Id.Id id,
            isPrivateEmail = isPrivateEmail,
            issuer = issuer,
            lastLoginAt = lastLoginAt,
            merchantId = Kernel.Types.Id.Id merchantId,
            personId = Kernel.Types.Id.Id personId,
            provider = provider,
            providerSubject = providerSubject,
            updatedAt = updatedAt,
            validTill = validTill
          }

instance ToTType' Beam.PersonSocialIdentity Domain.Types.PersonSocialIdentity.PersonSocialIdentity where
  toTType' (Domain.Types.PersonSocialIdentity.PersonSocialIdentity {..}) = do
    Beam.PersonSocialIdentityT
      { Beam.appleRefreshToken = appleRefreshToken <&> unEncrypted,
        Beam.clientId = clientId,
        Beam.createdAt = createdAt,
        Beam.email = email <&> unEncrypted,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isPrivateEmail = isPrivateEmail,
        Beam.issuer = issuer,
        Beam.lastLoginAt = lastLoginAt,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.provider = provider,
        Beam.providerSubject = providerSubject,
        Beam.updatedAt = updatedAt,
        Beam.validTill = validTill
      }
