module Components.DriverDetails.Controller where
import Components.PrimaryButton as PrimaryButtonController
import Components.SourceToDestination as SourceToDestinationController
import Screens.Types(Stage, ZoneType(..))
import Data.Maybe (Maybe)



data Action = NoAction
        

type DriverDetailsState = 
    { props :: DriverDetailsProps
    , data :: DriverDetailsData
    }
    
type DriverDetailsProps = 
    { currentStage :: Stage
    , zoneType :: ZoneType
  }

type DriverDetailsData = 
    { rideId :: String
    , firstName :: String
    , middleName :: String
    , lastName ::String
    , totalRidesAssigned :: Int
    , rating :: Number
    , totalUsersRated :: Int
    , language :: String
    , mobileNumber :: String
    , totalDistanceTravelled :: Int
    , homeTown :: String
    , lateNightTrips :: Int
    , totalCompletedTrips :: Int 
    , lastRegistered :: String
    }    