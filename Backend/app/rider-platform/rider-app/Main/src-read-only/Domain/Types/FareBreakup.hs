{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FareBreakup where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FareBreakup = FareBreakup
  { id :: Kernel.Types.Id.Id Domain.Types.FareBreakup.FareBreakup,
    description :: Kernel.Prelude.Text,
    amount :: Kernel.Types.Common.Price,
    entityType :: Domain.Types.FareBreakup.FareBreakupEntityType,
    entityId :: Kernel.Prelude.Text
  }
  deriving (Show)

data FareBreakupAPIEntity = FareBreakupAPIEntity {amount :: Kernel.Types.Common.Money, amountWithCurrency :: Kernel.Types.Common.PriceAPIEntity, description :: Kernel.Prelude.Text}
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data FareBreakupEntityType = BOOKING_UPDATE_REQUEST | BOOKING | RIDE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FareBreakupEntityType)
