module Components.DriverDetails.Controller where
import Screens.Types(Stage, ZoneType(..))
import Data.Maybe (Maybe)



data Action = Close | BackPressed | NoAction 
        

type DriverDetailsState = 
    { props :: DriverDetailsProps
    , data :: DriverDetailsData
    }
    
type DriverDetailsProps = 
    { currentStage :: Stage,
      zoneType :: ZoneType,
      isSpecialZone :: Boolean
     
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
    , lastRegistered ::  String
    
    }  

