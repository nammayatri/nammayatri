{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.FarePolicy.FarePolicySlabDetails.FarePolicySlabDetailsSlab
  ( module Storage.Beam.FarePolicySlabsDetailsSlab,
    FullFarePolicySlabsDetailsSlab,
  )
where

import Data.Proxy (Proxy (..))
import Domain.Types.UtilsTH
import Storage.Beam.FarePolicySlabsDetailsSlab
import Storage.Queries.FarePolicySlabsDetailsSlabExtra (FullFarePolicySlabsDetailsSlab)

$(mkCacParseInstanceList ''FarePolicySlabsDetailsSlab)
