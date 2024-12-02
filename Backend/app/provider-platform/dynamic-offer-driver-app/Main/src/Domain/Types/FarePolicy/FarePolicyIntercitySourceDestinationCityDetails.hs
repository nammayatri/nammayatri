{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FarePolicy.FarePolicyIntercitySourceDestinationCityDetails where

import Data.Aeson
import qualified Data.List.NonEmpty as NE
import Data.Ord (comparing)
import Domain.Types.Common
import Kernel.Prelude
import Kernel.Types.Common

data FarePolicyIntercitySourceDestinationCityDetailsD (s :: UsageSafety) = FarePolicyIntercitySourceDestinationCityDetails
  { destinationCity :: Kernel.Prelude.Text,
    sourceCity :: Kernel.Prelude.Text,
    stateEntryPermitCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    tollCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, Show, Eq, ToSchema)

type FarePolicyIntercitySourceDestinationCityDetails = FarePolicyIntercitySourceDestinationCityDetailsD 'Safe

instance FromJSON (FarePolicyIntercitySourceDestinationCityDetailsD 'Unsafe)

instance ToJSON (FarePolicyIntercitySourceDestinationCityDetailsD 'Unsafe)

instance FromJSON (FarePolicyIntercitySourceDestinationCityDetailsD 'Safe)

instance ToJSON (FarePolicyIntercitySourceDestinationCityDetailsD 'Safe)
