{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Domain.Types.EstimateStatus where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data EstimateStatus
  = NEW
  | DRIVER_QUOTE_REQUESTED
  | CANCELLED
  | GOT_DRIVER_QUOTE
  | DRIVER_QUOTE_CANCELLED
  | RIDE_SEARCH_EXPIRED
  | COMPLETED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''EstimateStatus)
