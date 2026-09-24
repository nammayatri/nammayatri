{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.FarePolicy.FarePolicyInterCityDetailsPricingSlabs
  ( module Storage.Beam.FarePolicyInterCityDetailsPricingSlabs,
    FullFarePolicyInterCityDetailsPricingSlabs,
  )
where

import Data.Proxy (Proxy (..))
import Domain.Types.UtilsTH
import Storage.Beam.FarePolicyInterCityDetailsPricingSlabs
import Storage.Queries.FarePolicyInterCityDetailsPricingSlabsExtra (FullFarePolicyInterCityDetailsPricingSlabs)

$(mkCacParseInstanceList ''FarePolicyInterCityDetailsPricingSlabs)
