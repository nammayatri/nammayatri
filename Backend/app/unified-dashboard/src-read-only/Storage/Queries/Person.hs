{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Person (module Storage.Queries.Person, module ReExport) where

import qualified Data.Text
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Person as Beam
import Storage.Queries.PersonExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Person.Person -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Person.Person] -> m ())
createMany = traverse_ create

findByEmailAndPassword ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash -> Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash -> m (Maybe Domain.Types.Person.Person))
findByEmailAndPassword emailHashBeam passwordHashBeam = do findOneWithKV [Se.And [Se.Is Beam.emailHash $ Se.Eq emailHashBeam, Se.Is Beam.passwordHash $ Se.Eq passwordHashBeam]]

findByEmailHash :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash -> m (Maybe Domain.Types.Person.Person))
findByEmailHash emailHashBeam = do findOneWithKV [Se.Is Beam.emailHash $ Se.Eq emailHashBeam]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.Person.Person))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByMobileNumberAndPassword ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash -> m (Maybe Domain.Types.Person.Person))
findByMobileNumberAndPassword mobileNumberHashBeam mobileCountryCode passwordHashBeam = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.mobileNumberHash $ Se.Eq mobileNumberHashBeam,
          Se.Is Beam.mobileCountryCode $ Se.Eq mobileCountryCode,
          Se.Is Beam.passwordHash $ Se.Eq passwordHashBeam
        ]
    ]

findByMobileNumberHash ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash -> Kernel.Prelude.Maybe Data.Text.Text -> m (Maybe Domain.Types.Person.Person))
findByMobileNumberHash mobileNumberHashBeam mobileCountryCode = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.mobileNumberHash $ Se.Eq mobileNumberHashBeam,
          Se.Is Beam.mobileCountryCode $ Se.Eq mobileCountryCode
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.Person.Person))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Person.Person -> m ())
updateByPrimaryKey (Domain.Types.Person.Person {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.approvedBy (Kernel.Types.Id.getId <$> approvedBy),
      Se.Set Beam.emailEncrypted (((email <&> unEncrypted . (.encrypted)))),
      Se.Set Beam.emailHash ((email <&> (.hash))),
      Se.Set Beam.firstName firstName,
      Se.Set Beam.lastName lastName,
      Se.Set Beam.mobileCountryCode mobileCountryCode,
      Se.Set Beam.mobileNumberEncrypted (((mobileNumber <&> unEncrypted . (.encrypted)))),
      Se.Set Beam.mobileNumberHash ((mobileNumber <&> (.hash))),
      Se.Set Beam.passwordHash passwordHash,
      Se.Set Beam.passwordUpdatedAt passwordUpdatedAt,
      Se.Set Beam.receiveNotification receiveNotification,
      Se.Set Beam.rejectedAt rejectedAt,
      Se.Set Beam.rejectedBy (Kernel.Types.Id.getId <$> rejectedBy),
      Se.Set Beam.rejectionReason rejectionReason,
      Se.Set Beam.roleId (Kernel.Types.Id.getId roleId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.verified verified
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
