{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.IGMIssue where

import qualified Data.Text
import qualified Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data IGMIssue = IGMIssue
  { createdAt :: Kernel.Prelude.UTCTime,
    customerEmail :: Kernel.Prelude.Maybe Data.Text.Text,
    customerName :: Kernel.Prelude.Maybe Data.Text.Text,
    customerPhone :: Kernel.Prelude.Maybe Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.IGMIssue.IGMIssue,
    issueRaisedByMerchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    issueRaisedByName :: Kernel.Prelude.Maybe Data.Text.Text,
    issueStatus :: Domain.Types.IGMIssue.Status,
    issueType :: Data.Text.Text,
    respondentAction :: Kernel.Prelude.Maybe Data.Text.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data Status = OPEN | CLOSED | ESCALATED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''Status))
