{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffers
  ( module Storage.Beam.FarePolicyRentalDetailsDistanceBuffers,
    FullFarePolicyRentalDetailsDistanceBuffers,
  )
where

import Data.Proxy (Proxy (..))
import Domain.Types.UtilsTH
import Storage.Beam.FarePolicyRentalDetailsDistanceBuffers
import Storage.Queries.FarePolicyRentalDetailsDistanceBuffersExtra (FullFarePolicyRentalDetailsDistanceBuffers)

$(mkCacParseInstanceList ''FarePolicyRentalDetailsDistanceBuffers)
