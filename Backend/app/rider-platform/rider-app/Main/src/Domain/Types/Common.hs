{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Common
  ( module Domain.Types,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.OnDemand.Enums as Enums
import Domain.Types
import qualified Domain.Types.VehicleCategory as DTVC
import qualified Domain.Types.VehicleVariant as DTVV
import Tools.Beam.UtilsTH

$(mkBeamInstancesForEnumAndList ''MultimodalTravelMode)
$(mkBeamInstancesForEnum ''FareProductType)
$(mkBeamInstancesForEnumAndList ''ServiceTierType)
$(mkBeamInstancesForEnumAndList ''DTVV.VehicleVariant)
$(mkBeamInstancesForEnumAndList ''DTVC.VehicleCategory)
$(mkBeamInstancesForEnumAndList ''Enums.VehicleCategory)
$(mkBeamInstancesForEnum ''TripCategory)
$(mkBeamInstancesForEnum ''TripParty)
$(mkBeamInstancesForEnumAndList ''Spec.VehicleCategory)
$(mkBeamInstancesForEnumAndList ''Spec.ServiceTierType)
$(mkBeamInstancesForEnumAndList ''Spec.Network)
