{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SearchReqLocation where

import Data.Aeson
import qualified Domain.Types.LocationAddress
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data SearchReqLocation = SearchReqLocation
  { address :: Domain.Types.LocationAddress.LocationAddress,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.SearchReqLocation.SearchReqLocation,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Kernel.External.Maps.HasCoordinates)

data SearchReqLocationAPIEntity = SearchReqLocationAPIEntity {lat :: Kernel.Prelude.Double, lon :: Kernel.Prelude.Double} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
