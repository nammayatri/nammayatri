{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleInfo where

import qualified Domain.Types.VehicleInfo
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleInfo as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleInfo.VehicleInfo -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VehicleInfo.VehicleInfo] -> m ())
createMany = traverse_ create

deleteAllByRcId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m ())
deleteAllByRcId rcId = do deleteWithKV [Se.Is Beam.rcId $ Se.Eq (Kernel.Types.Id.getId rcId)]

findAllByRcId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m [Domain.Types.VehicleInfo.VehicleInfo])
findAllByRcId rcId = do findAllWithKV [Se.Is Beam.rcId $ Se.Eq (Kernel.Types.Id.getId rcId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate -> m (Maybe Domain.Types.VehicleInfo.VehicleInfo))
findByPrimaryKey questionId rcId = do findOneWithKV [Se.And [Se.Is Beam.questionId $ Se.Eq questionId, Se.Is Beam.rcId $ Se.Eq (Kernel.Types.Id.getId rcId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleInfo.VehicleInfo -> m ())
updateByPrimaryKey (Domain.Types.VehicleInfo.VehicleInfo {..}) = do
  updateWithKV
    [Se.Set Beam.answer answer, Se.Set Beam.question question]
    [ Se.And
        [ Se.Is Beam.questionId $ Se.Eq questionId,
          Se.Is Beam.rcId $ Se.Eq (Kernel.Types.Id.getId rcId)
        ]
    ]

instance FromTType' Beam.VehicleInfo Domain.Types.VehicleInfo.VehicleInfo where
  fromTType' (Beam.VehicleInfoT {..}) = do pure $ Just Domain.Types.VehicleInfo.VehicleInfo {answer = answer, question = question, questionId = questionId, rcId = Kernel.Types.Id.Id rcId}

instance ToTType' Beam.VehicleInfo Domain.Types.VehicleInfo.VehicleInfo where
  toTType' (Domain.Types.VehicleInfo.VehicleInfo {..}) = do Beam.VehicleInfoT {Beam.answer = answer, Beam.question = question, Beam.questionId = questionId, Beam.rcId = Kernel.Types.Id.getId rcId}
