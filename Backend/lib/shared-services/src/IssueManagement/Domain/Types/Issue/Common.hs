{-# LANGUAGE TemplateHaskell #-}

module IssueManagement.Domain.Types.Issue.Common where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Lib.UtilsTH

-- Tag the options which will be used for filtering
data FilterOptionTags = OnUserBlocked | OnNoFareDiscrepancy
  deriving (Generic, FromJSON, ToJSON, Show, Eq, Read, Ord, ToSchema)

-- Which filter to apply on options
data FilterFn = UserBlocked | NoFareDiscrepancy
  deriving (Generic, FromJSON, ToJSON, Show, Eq, Read, Ord, ToSchema)

$(mkBeamInstancesForEnumAndList ''FilterOptionTags)

$(mkBeamInstancesForEnumAndList ''FilterFn)

data UserInput = Vpa Text | Amount Int | PlainText Text
  deriving (Generic, FromJSON, ToJSON, Show, Eq, Read, Ord, ToSchema)
