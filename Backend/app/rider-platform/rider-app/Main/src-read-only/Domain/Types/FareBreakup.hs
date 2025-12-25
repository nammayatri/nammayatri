{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FareBreakup where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FareBreakup = FareBreakup
  { amount :: Kernel.Types.Common.Price,
    description :: Kernel.Prelude.Text,
    entityId :: Kernel.Prelude.Text,
    entityType :: Domain.Types.FareBreakup.FareBreakupEntityType,
    id :: Kernel.Types.Id.Id Domain.Types.FareBreakup.FareBreakup
  }
  deriving (Show)

data FareBreakupAPIEntity = FareBreakupAPIEntity {amount :: Kernel.Types.Common.Money, amountWithCurrency :: Kernel.Types.Common.PriceAPIEntity, description :: Kernel.Prelude.Text}
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data FareBreakupEntityType = BOOKING_UPDATE_REQUEST | BOOKING | RIDE | INITIAL_BOOKING deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FareBreakupEntityType)
