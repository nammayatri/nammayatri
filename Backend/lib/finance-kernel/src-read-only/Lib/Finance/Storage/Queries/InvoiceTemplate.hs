{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.InvoiceTemplate (module Lib.Finance.Storage.Queries.InvoiceTemplate, module ReExport) where

import qualified Domain.Types.Invoice
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.InvoiceTemplate
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.InvoiceTemplate as Beam
import Lib.Finance.Storage.Queries.InvoiceTemplateExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.InvoiceTemplate.InvoiceTemplate -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.InvoiceTemplate.InvoiceTemplate] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.InvoiceTemplate.InvoiceTemplate]))
findAllByMerchantOpCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

findByMerchantOpCityIdInvoiceTypeAndLanguage ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.Invoice.InvoiceType -> Kernel.External.Types.Language -> m (Maybe Lib.Finance.Domain.Types.InvoiceTemplate.InvoiceTemplate))
findByMerchantOpCityIdInvoiceTypeAndLanguage merchantOperatingCityId invoiceType language = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.invoiceType $ Se.Eq invoiceType,
          Se.Is Beam.language $ Se.Eq language
        ]
    ]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.InvoiceTemplate.InvoiceTemplate -> m (Maybe Lib.Finance.Domain.Types.InvoiceTemplate.InvoiceTemplate))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.InvoiceTemplate.InvoiceTemplate -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.InvoiceTemplate.InvoiceTemplate {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.invoiceType invoiceType,
      Se.Set Beam.language language,
      Se.Set Beam.lineItemRowTemplate lineItemRowTemplate,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.template template,
      Se.Set Beam.totalsLineRowTemplate totalsLineRowTemplate,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
