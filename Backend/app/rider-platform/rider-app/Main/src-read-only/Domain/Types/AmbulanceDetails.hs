{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AmbulanceDetails (module Domain.Types.AmbulanceDetails, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.AmbulanceDetails as ReExport
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.GenericPretty
import qualified Tools.Beam.UtilsTH

data AmbulanceDetails = AmbulanceDetails
  { ambulanceQuoteBreakupList :: [Domain.Types.AmbulanceDetails.AmbulanceQuoteBreakup],
    id :: Kernel.Types.Id.Id Domain.Types.AmbulanceDetails.AmbulanceDetails,
    maxEstimatedFare :: Kernel.Types.Common.Price,
    minEstimatedFare :: Kernel.Types.Common.Price
  }
  deriving (Generic, Show)

data AmbulanceQuoteBreakup = AmbulanceQuoteBreakup
  { id :: Kernel.Types.Id.Id Domain.Types.AmbulanceDetails.AmbulanceQuoteBreakup,
    price :: Domain.Types.AmbulanceDetails.AmbulanceQuoteBreakupPrice,
    quoteId :: Kernel.Types.Id.Id Domain.Types.AmbulanceDetails.AmbulanceDetails,
    title :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, Kernel.Utils.GenericPretty.PrettyShow)

newtype AmbulanceQuoteBreakupPrice = AmbulanceQuoteBreakupPrice {value :: Kernel.Types.Common.Price} deriving (Generic, Show, Kernel.Utils.GenericPretty.PrettyShow)
