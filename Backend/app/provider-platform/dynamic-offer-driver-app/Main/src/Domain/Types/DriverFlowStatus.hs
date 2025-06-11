module Domain.Types.DriverFlowStatus where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Utils.TH
import Tools.Beam.UtilsTH

data DriverFlowStatus = ONLINE | OFFLINE | SILENT | ON_PICKUP | ON_RIDE | ACTIVE | INACTIVE
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

instance CH.ClickhouseValue DriverFlowStatus

$(mkHttpInstancesForEnum ''DriverFlowStatus)

$(mkBeamInstancesForEnumAndList ''DriverFlowStatus)
