{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.InitiatedBy where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import qualified Tools.Beam.UtilsTH

data InitiatedBy = DriverApp | FleetDashboard | AdminDashboard | System | BotFlow | Operator
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''InitiatedBy)

$(mkHttpInstancesForEnum ''InitiatedBy)
