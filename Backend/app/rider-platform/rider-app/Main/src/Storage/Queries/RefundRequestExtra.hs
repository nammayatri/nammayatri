module Storage.Queries.RefundRequestExtra where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified "this" Domain.Types.Person as DP
import qualified Domain.Types.RefundRequest as DRefundRequest
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Sequelize as Se
import qualified Storage.Beam.RefundRequest as BeamRR
import Storage.Queries.OrphanInstances.RefundRequest ()

findAllRefundRequestItem ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Int ->
  Int ->
  Maybe DRefundRequest.RefundRequestStatus ->
  Maybe DRefundRequest.RefundRequestCode ->
  Maybe (Id DP.Person) ->
  Maybe (Id DPaymentOrder.PaymentOrder) ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  m [DRefundRequest.RefundRequest]
findAllRefundRequestItem merchantOpCityId limit offset mbStatus mbCode mbPersonId mbOrderId mbFrom mbTo = do
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is BeamRR.merchantOperatingCityId $ Se.Eq merchantOpCityId.getId]
          <> maybe [] (\status -> [Se.Is BeamRR.status $ Se.Eq status]) mbStatus
          <> maybe [] (\code -> [Se.Is BeamRR.code $ Se.Eq code]) mbCode
          <> maybe [] (\personId -> [Se.Is BeamRR.personId $ Se.Eq personId.getId]) mbPersonId
          <> maybe [] (\orderId -> [Se.Is BeamRR.orderId $ Se.Eq orderId.getId]) mbOrderId
          <> maybe [] (\from -> [Se.Is BeamRR.createdAt $ Se.GreaterThanOrEq from]) mbFrom
          <> maybe [] (\to -> [Se.Is BeamRR.createdAt $ Se.LessThanOrEq to]) mbTo
    ]
    (Se.Desc BeamRR.createdAt)
    (Just limit)
    (Just offset)
