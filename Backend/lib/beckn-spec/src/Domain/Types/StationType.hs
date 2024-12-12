{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.StationType where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data StationType = START | END | TRANSIT | INTERMEDIATE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''StationType)

data StationCategory = MULTIMODAL_STATION | INDEPENDENT_STATION deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''StationCategory)
$(mkBeamInstancesForEnumAndList ''StationCategory)
