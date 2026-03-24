{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.UserPreferredRoute where
import Kernel.Prelude
import Data.Aeson
import qualified Domain.Types.LocationAddress
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Tools.Beam.UtilsTH



data UserPreferredRoute
    = UserPreferredRoute {createdAt :: Kernel.Prelude.UTCTime,
                          fromLocationAddress :: Domain.Types.LocationAddress.LocationAddress,
                          fromLocationLat :: Kernel.Prelude.Double,
                          fromLocationLon :: Kernel.Prelude.Double,
                          id :: Kernel.Types.Id.Id Domain.Types.UserPreferredRoute.UserPreferredRoute,
                          personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                          priority :: Kernel.Prelude.Int,
                          routeName :: Kernel.Prelude.Text,
                          toLocationAddress :: Domain.Types.LocationAddress.LocationAddress,
                          toLocationLat :: Kernel.Prelude.Double,
                          toLocationLon :: Kernel.Prelude.Double,
                          updatedAt :: Kernel.Prelude.UTCTime,
                          usageCount :: Kernel.Prelude.Int}
    deriving (Generic, ( Show), ( Eq), ( FromJSON), ( ToJSON), ( ToSchema))



