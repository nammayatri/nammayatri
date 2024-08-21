{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SearchRequestDeliveryDetails where

import qualified BecknV2.OnDemand.Enums
import Data.Aeson
import qualified Domain.Types.DeliveryPersonDetails
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH

data SearchRequestDeliveryDetails = SearchRequestDeliveryDetails
  { initiatedAs :: BecknV2.OnDemand.Enums.DeliveryInitiation,
    receiverDetails :: Domain.Types.DeliveryPersonDetails.DeliveryPersonDetails,
    searchRequestId :: Kernel.Prelude.Text,
    senderDetails :: Domain.Types.DeliveryPersonDetails.DeliveryPersonDetails,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)
