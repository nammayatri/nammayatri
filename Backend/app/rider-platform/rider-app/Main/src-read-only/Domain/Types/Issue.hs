{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Issue where

import Data.Aeson
import qualified Domain.Types.Person
import qualified Domain.Types.Quote
import qualified IssueManagement.Common
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Issue = Issue
  { id :: Kernel.Types.Id.Id Domain.Types.Issue.Issue,
    customerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    bookingId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Quote.Quote),
    contactEmail :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reason :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Text,
    ticketId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: IssueManagement.Common.IssueStatus,
    nightSafety :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
