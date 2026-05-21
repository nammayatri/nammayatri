{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.InvoiceTemplate where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.InvoiceTemplate
import qualified Lib.Finance.Storage.Beam.InvoiceTemplate as Beam

instance FromTType' Beam.InvoiceTemplate Lib.Finance.Domain.Types.InvoiceTemplate.InvoiceTemplate where
  fromTType' (Beam.InvoiceTemplateT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.InvoiceTemplate.InvoiceTemplate
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            invoiceType = invoiceType,
            language = language,
            lineItemRowTemplate = lineItemRowTemplate,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            template = template,
            totalsLineRowTemplate = totalsLineRowTemplate,
            updatedAt = updatedAt
          }

instance ToTType' Beam.InvoiceTemplate Lib.Finance.Domain.Types.InvoiceTemplate.InvoiceTemplate where
  toTType' (Lib.Finance.Domain.Types.InvoiceTemplate.InvoiceTemplate {..}) = do
    Beam.InvoiceTemplateT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.invoiceType = invoiceType,
        Beam.language = language,
        Beam.lineItemRowTemplate = lineItemRowTemplate,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.template = template,
        Beam.totalsLineRowTemplate = totalsLineRowTemplate,
        Beam.updatedAt = updatedAt
      }
