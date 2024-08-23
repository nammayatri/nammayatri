{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FareBreakup (module Domain.Types.FareBreakup, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.FareBreakup as ReExport
import qualified Domain.Types.FareBreakupTitle
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FareBreakup = FareBreakup
  { amount :: Kernel.Types.Common.Price,
    description :: Kernel.Prelude.Text,
    entityId :: Kernel.Prelude.Text,
    entityType :: Domain.Types.FareBreakup.FareBreakupEntityType,
    id :: Kernel.Types.Id.Id Domain.Types.FareBreakup.FareBreakup,
    title :: Kernel.Prelude.Maybe Domain.Types.FareBreakupTitle.FareBreakupTitle
  }
  deriving (Show)

data FareBreakupAPIEntity = FareBreakupAPIEntity {amount :: Kernel.Types.Common.Money, amountWithCurrency :: Kernel.Types.Common.PriceAPIEntity, description :: Kernel.Prelude.Text}
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data FareBreakupEntityType = BOOKING_UPDATE_REQUEST | BOOKING | RIDE | INITIAL_BOOKING deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''FareBreakupEntityType)
