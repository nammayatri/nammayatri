{-
  Finance.Audit.Interface

  Input types for audit operations.
  The actual operations are in Lib.Finance.Audit.Service
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Audit.Interface
  ( AuditInput (..),
  )
where

import Data.Aeson (Value)
import Kernel.Prelude
import Lib.Finance.Domain.Types.AuditEntry (AuditAction)

-- | Input for creating an audit entry
data AuditInput = AuditInput
  { entityType :: Text,
    entityId :: Text,
    action :: AuditAction,
    actorType :: Text,
    actorId :: Maybe Text,
    beforeState :: Maybe Value, -- Maps to previousState in DB
    afterState :: Maybe Value, -- Maps to newState in DB
    merchantId :: Text,
    merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic)
