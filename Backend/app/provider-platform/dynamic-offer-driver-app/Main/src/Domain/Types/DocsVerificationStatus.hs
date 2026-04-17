{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.Types.DocsVerificationStatus where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Tools.Beam.UtilsTH (mkBeamInstancesForEnumAndList)

data DocsVerificationStatus
  = ADMIN_PENDING
  | ADMIN_APPROVED
  | ADMIN_REJECTED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnumAndList ''DocsVerificationStatus)
$(mkHttpInstancesForEnum ''DocsVerificationStatus)
