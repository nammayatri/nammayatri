{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection
  ( module Storage.Beam.FarePolicyProgressiveDetailsPerExtraKmRateSection,
    FullFarePolicyProgressiveDetailsPerExtraKmRateSection,
  )
where

import Data.Proxy (Proxy (..))
import Domain.Types.UtilsTH
import Storage.Beam.FarePolicyProgressiveDetailsPerExtraKmRateSection
import Storage.Queries.FarePolicyProgressiveDetailsPerExtraKmRateSectionExtra (FullFarePolicyProgressiveDetailsPerExtraKmRateSection)

$(mkCacParseInstanceList ''FarePolicyProgressiveDetailsPerExtraKmRateSection)
