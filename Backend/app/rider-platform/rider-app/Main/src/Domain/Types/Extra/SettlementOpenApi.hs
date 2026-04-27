{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan @ToSchema@ for OpenAPI generation (types live in mobility-core / finance-kernel).
module Domain.Types.Extra.SettlementOpenApi () where

import Data.OpenApi (ToSchema)
import Kernel.External.Settlement.Types (SettlementParserTypeMap, SettlementService, SplitSettlementCustomerType)
import Kernel.Prelude
import Lib.Finance.Settlement.Ingestion (IngestionResult)

deriving anyclass instance ToSchema SettlementService

deriving anyclass instance ToSchema SplitSettlementCustomerType

deriving anyclass instance ToSchema SettlementParserTypeMap

deriving anyclass instance ToSchema IngestionResult
