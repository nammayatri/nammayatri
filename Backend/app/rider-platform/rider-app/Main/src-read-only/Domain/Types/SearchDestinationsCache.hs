{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SearchDestinationsCache where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data SearchDestinationsCache = SearchDestinationsCache
  { geoHash :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.SearchDestinationsCache.SearchDestinationsCache,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    response :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
