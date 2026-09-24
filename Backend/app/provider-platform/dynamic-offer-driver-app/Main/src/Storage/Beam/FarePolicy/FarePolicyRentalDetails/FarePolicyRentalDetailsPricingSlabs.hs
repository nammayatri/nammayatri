{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsPricingSlabs
  ( module Storage.Beam.FarePolicyRentalDetailsPricingSlabs,
    FullFarePolicyRentalDetailsPricingSlabs,
  )
where

import Data.Proxy (Proxy (..))
import Domain.Types.UtilsTH
import Storage.Beam.FarePolicyRentalDetailsPricingSlabs
import Storage.Queries.FarePolicyRentalDetailsPricingSlabsExtra (FullFarePolicyRentalDetailsPricingSlabs)

$(mkCacParseInstanceList ''FarePolicyRentalDetailsPricingSlabs)
