{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Common
  ( module Domain.Types,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.OnDemand.Enums as Enums
import Domain.Types
import qualified Domain.Types.BookingStatus as DTBS
import qualified Domain.Types.EstimateStatus as DTES
import qualified Domain.Types.FRFSQuoteCategoryType as DTFRFSQuoteCategoryType
import qualified Domain.Types.FRFSTicketBookingStatus as DTFRFSTicketBookingStatus
import qualified Domain.Types.FRFSTicketStatus as DTFRFSTicketStatus
import qualified Domain.Types.ParcelType as DTPT
import qualified Domain.Types.RideStatus as DTRS
import qualified Domain.Types.StationType as DTS
import qualified Domain.Types.VehicleCategory as DTVC
import qualified Domain.Types.VehicleVariant as DTVV
import qualified Kernel.Storage.ClickhouseV2 as CH
import Tools.Beam.UtilsTH

$(mkBeamInstancesForEnumAndList ''MultimodalTravelMode)
$(mkBeamInstancesForEnumAndList ''DTS.SuggestedStations)
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
$(mkBeamInstancesForEnumAndList ''DTPT.ParcelType)
$(mkBeamInstancesForEnumAndList ''DTFRFSTicketBookingStatus.FRFSTicketBookingStatus)
$(mkBeamInstancesForEnum ''DTBS.BookingStatus)
$(mkBeamInstancesForEnumAndList ''DTRS.RideStatus)
$(mkBeamInstancesForEnum ''DTFRFSTicketStatus.FRFSTicketStatus)
$(mkBeamInstancesForEnumAndList ''DTES.EstimateStatus)
$(mkBeamInstancesForEnumAndList ''DTFRFSQuoteCategoryType.FRFSQuoteCategoryType)

instance CH.ClickhouseValue DTBS.BookingStatus
