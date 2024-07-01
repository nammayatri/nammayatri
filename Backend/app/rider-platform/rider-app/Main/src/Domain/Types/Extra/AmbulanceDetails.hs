{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.AmbulanceDetails where

import Domain.Types.Extra.RentalDetails
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.JSON (removeNullFields)

data AmbulanceDetailsAPIEntity = AmbulanceDetailsAPIEntity
  { minEstimatedFare :: PriceAPIEntity,
    maxEstimatedFare :: PriceAPIEntity,
    ambulanceQuoteBreakupList :: [AmbulanceQuoteBreakupAPIEntity],
    tollCharges :: Maybe PriceAPIEntity
  }
  deriving (Generic, FromJSON, Show, ToSchema)

data AmbulanceQuoteBreakupAPIEntity = AmbulanceQuoteBreakupAPIEntity
  { title :: Text,
    price :: PriceAPIEntity
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance ToJSON AmbulanceDetailsAPIEntity where
  toJSON = genericToJSON removeNullFields
