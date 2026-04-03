{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.SearchReqLocation where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.External.Maps
import qualified Domain.Types.LocationAddress
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH



data SearchReqLocation
    = SearchReqLocation {address :: Domain.Types.LocationAddress.LocationAddress,
                         createdAt :: Kernel.Prelude.UTCTime,
                         id :: Kernel.Types.Id.Id Domain.Types.SearchReqLocation.SearchReqLocation,
                         lat :: Kernel.Prelude.Double,
                         lon :: Kernel.Prelude.Double,
                         updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, ( Show), ( Kernel.External.Maps.HasCoordinates))
data SearchReqLocationAPIEntity = SearchReqLocationAPIEntity {lat :: Kernel.Prelude.Double, lon :: Kernel.Prelude.Double} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



