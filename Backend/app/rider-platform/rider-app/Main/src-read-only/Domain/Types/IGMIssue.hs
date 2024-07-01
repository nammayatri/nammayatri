{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.IGMIssue where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Booking
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data IGMIssue = IGMIssue
  { bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.IGMIssue.IGMIssue,
    issueStatus :: Domain.Types.IGMIssue.Status,
    issueType :: Domain.Types.IGMIssue.IssueType,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    respondentAction :: Kernel.Prelude.Maybe Data.Text.Text,
    respondentEmail :: Kernel.Prelude.Maybe Data.Text.Text,
    respondentEntityType :: Kernel.Prelude.Maybe Domain.Types.IGMIssue.Entity,
    respondentName :: Kernel.Prelude.Maybe Data.Text.Text,
    respondentPhone :: Kernel.Prelude.Maybe Data.Text.Text,
    respondingMerchantId :: Data.Text.Text,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    transactionId :: Data.Text.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data Entity = GRO | COUNTERPARTY deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data IssueType = GRIEVANCE | ISSUE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data Status = OPEN | CLOSED | ESCALATED | RESOLVED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''Entity)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''IssueType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''Status)
