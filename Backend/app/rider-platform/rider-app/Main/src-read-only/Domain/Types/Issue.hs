{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Issue where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Quote
import qualified IssueManagement.Common
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Issue = Issue
  { bookingId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Quote.Quote),
    contactEmail :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    customerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    description :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Issue.Issue,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    nightSafety :: Kernel.Prelude.Bool,
    reason :: Kernel.Prelude.Text,
    status :: IssueManagement.Common.IssueStatus,
    ticketId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
