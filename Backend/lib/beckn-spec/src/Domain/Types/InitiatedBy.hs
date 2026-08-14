{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.InitiatedBy (InitiatedBy (..)) where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data InitiatedBy = DriverApp | FleetDashboard | AdminDashboard | System | BotFlow | Operator | Website
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnumAndList ''InitiatedBy)

$(mkHttpInstancesForEnum ''InitiatedBy)
