{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BBPS (module Storage.Queries.BBPS, module ReExport) where

import qualified Domain.Types.BBPS
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BBPS as Beam
import Storage.Queries.BBPSExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BBPS.BBPS -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BBPS.BBPS] -> m ())
createMany = traverse_ create

findAllByCustomerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.BBPS.BBPS])
findAllByCustomerId limit offset customerId = do findAllWithOptionsKV [Se.Is Beam.customerId $ Se.Eq (Kernel.Types.Id.getId customerId)] (Se.Desc Beam.createdAt) limit offset

findByRefId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BBPS.BBPS -> m (Maybe Domain.Types.BBPS.BBPS))
findByRefId refId = do findOneWithKV [Se.Is Beam.refId $ Se.Eq (Kernel.Types.Id.getId refId)]

updatePaymentInformationAndStatusByRefId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.BBPS.BBPSPaymentMode -> Kernel.Prelude.Maybe [Domain.Types.BBPS.Tag] -> Domain.Types.BBPS.BBPSPaymentStatus -> Kernel.Types.Id.Id Domain.Types.BBPS.BBPS -> m ())
updatePaymentInformationAndStatusByRefId paymentTxnId paymentMode paymentInformation status refId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.paymentTxnId paymentTxnId,
      Se.Set Beam.paymentMode paymentMode,
      Se.Set Beam.paymentInformation (Kernel.Prelude.toJSON <$> paymentInformation),
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.refId $ Se.Eq (Kernel.Types.Id.getId refId)]

updateStatusAndErrorMessageByRefId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.BBPS.BBPSPaymentStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.BBPS.BBPS -> m ())
updateStatusAndErrorMessageByRefId status errorMessage refId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.errorMessage errorMessage, Se.Set Beam.updatedAt _now] [Se.Is Beam.refId $ Se.Eq (Kernel.Types.Id.getId refId)]

updateStatusByRefId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BBPS.BBPSPaymentStatus -> Kernel.Types.Id.Id Domain.Types.BBPS.BBPS -> m ())
updateStatusByRefId status refId = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.refId $ Se.Eq (Kernel.Types.Id.getId refId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BBPS.BBPS -> m (Maybe Domain.Types.BBPS.BBPS))
findByPrimaryKey refId = do findOneWithKV [Se.And [Se.Is Beam.refId $ Se.Eq (Kernel.Types.Id.getId refId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BBPS.BBPS -> m ())
updateByPrimaryKey (Domain.Types.BBPS.BBPS {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.billerId billerId,
      Se.Set Beam.customerId (Kernel.Types.Id.getId customerId),
      Se.Set Beam.customerMobileNumber customerMobileNumber,
      Se.Set Beam.customerParams (Kernel.Prelude.toJSON <$> customerParams),
      Se.Set Beam.errorMessage errorMessage,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.paymentInformation (Kernel.Prelude.toJSON <$> paymentInformation),
      Se.Set Beam.paymentMode paymentMode,
      Se.Set Beam.paymentTxnId paymentTxnId,
      Se.Set Beam.refShortId (Kernel.Types.Id.getShortId refShortId),
      Se.Set Beam.status status,
      Se.Set Beam.transType transType,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.refId $ Se.Eq (Kernel.Types.Id.getId refId)]]
