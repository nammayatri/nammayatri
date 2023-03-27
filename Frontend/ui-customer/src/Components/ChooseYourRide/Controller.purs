module Components.ChooseYourRide.Controller where
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.ChooseVehicle.Controller as ChooseVehicleController
import PrestoDOM (Margin(..))
data Action = NoAction | ChooseAcVehicleController  ChooseVehicleController.Action | ChooseNonAcVehicleController ChooseVehicleController.Action | PrimaryButtonActionController PrimaryButtonController.Action

type Config = {
    mainText :: String,
    rideDistance :: String,
    rideDuration :: String, 
    vehicleCard1 :: Vehicle,
    vehicleCard2 :: Vehicle,
    selectedCar1 :: Boolean,
    primaryButtonText :: String
}

config :: Config 
config = {
    mainText : "Choose Your Ride",
    rideDistance : "",
    rideDuration : "",
    vehicleCard1 : {
    mainText : "Non Ac Taxi",
    subText1 : "Economical",
    subText2 : "4 people",
    fare     : "146",
    imageUrl : "ny_ic_sedan_yellow",
    margin   : (MarginHorizontal 10 8),
    ridefare : "146"
    },
    vehicleCard2 : {
    mainText : "Ac Taxi",
    subText1 : "Comfy",
    subText2 : "4 people",
    fare     : "146",
    imageUrl : "ny_ic_sedan_white",
    margin   : (MarginHorizontal 10 8),
    ridefare : "146"
    },
    selectedCar1 : true,
    primaryButtonText : "Confirm & Book" 
}

type Vehicle = {
    mainText :: String,
    subText1 :: String,
    subText2 :: String,
    fare     :: String,
    imageUrl :: String,
    margin   :: Margin,
    ridefare :: String
}
