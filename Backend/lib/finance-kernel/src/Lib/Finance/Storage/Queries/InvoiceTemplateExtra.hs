{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.InvoiceTemplateExtra where

import Data.Aeson (FromJSON, ToJSON)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Lib.Finance.Domain.Types.InvoiceTemplate (InvoiceTemplate)
import Lib.Finance.Storage.Queries.OrphanInstances.InvoiceTemplate

-- ToJSON/FromJSON needed for Redis caching via DynamicLogic.findOneConfigWithCacheKey.
deriving instance ToJSON InvoiceTemplate

deriving instance FromJSON InvoiceTemplate
