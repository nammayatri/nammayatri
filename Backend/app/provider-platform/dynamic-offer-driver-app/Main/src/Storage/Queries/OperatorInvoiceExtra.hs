module Storage.Queries.OperatorInvoiceExtra where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.OperatorInvoice as DOI
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Beam.OperatorInvoice as BeamOI
import qualified Sequelize as Se

-- | Find all invoices for an operator with filters, pagination, and ordering
findAllByOperatorWithFilters ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe DOI.OperatorInvoiceType ->
  Maybe DOI.OperatorInvoiceStatus ->
  Maybe (Id DP.Person) ->
  Int ->
  Int ->
  m [DOI.OperatorInvoice]
findAllByOperatorWithFilters operatorId merchantId mbFrom mbTo mbInvoiceType mbStatus mbFleetOwnerId limitVal offsetVal = do
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is BeamOI.operatorId $ Se.Eq (getId operatorId),
          Se.Is BeamOI.merchantId $ Se.Eq (getId merchantId)
        ]
          <> maybe [] (\from -> [Se.Is BeamOI.invoiceDate $ Se.GreaterThanOrEq from]) mbFrom
          <> maybe [] (\to -> [Se.Is BeamOI.invoiceDate $ Se.LessThanOrEq to]) mbTo
          <> maybe [] (\it -> [Se.Is BeamOI.invoiceType $ Se.Eq it]) mbInvoiceType
          <> maybe [] (\s -> [Se.Is BeamOI.status $ Se.Eq s]) mbStatus
          <> maybe [] (\fid -> [Se.Is BeamOI.fleetOwnerId $ Se.Eq (Just (getId fid))]) mbFleetOwnerId
    ]
    (Se.Desc BeamOI.createdAt)
    (Just limitVal)
    (Just offsetVal)

-- | Count invoices for an operator with the same filters
countByOperatorWithFilters ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe DOI.OperatorInvoiceType ->
  Maybe DOI.OperatorInvoiceStatus ->
  Maybe (Id DP.Person) ->
  m Int
countByOperatorWithFilters operatorId merchantId mbFrom mbTo mbInvoiceType mbStatus mbFleetOwnerId = do
  results <- findAllWithOptionsKV
    [ Se.And $
        [ Se.Is BeamOI.operatorId $ Se.Eq (getId operatorId),
          Se.Is BeamOI.merchantId $ Se.Eq (getId merchantId)
        ]
          <> maybe [] (\from -> [Se.Is BeamOI.invoiceDate $ Se.GreaterThanOrEq from]) mbFrom
          <> maybe [] (\to -> [Se.Is BeamOI.invoiceDate $ Se.LessThanOrEq to]) mbTo
          <> maybe [] (\it -> [Se.Is BeamOI.invoiceType $ Se.Eq it]) mbInvoiceType
          <> maybe [] (\s -> [Se.Is BeamOI.status $ Se.Eq s]) mbStatus
          <> maybe [] (\fid -> [Se.Is BeamOI.fleetOwnerId $ Se.Eq (Just (getId fid))]) mbFleetOwnerId
    ]
    Se.Asc -- ordering doesn't matter for count
    Nothing
    Nothing
  pure $ length results
