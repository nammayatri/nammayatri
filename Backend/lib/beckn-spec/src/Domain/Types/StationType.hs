module Domain.Types.StationType where

import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Prelude

data StationType = START | END | TRANSIT | INTERMEDIATE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data SuggestedStations = SuggestedStations
  { towardsStation :: Text,
    index :: Int,
    toggableIndex :: Int,
    stationsList :: [Text]
  }
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''StationType)
