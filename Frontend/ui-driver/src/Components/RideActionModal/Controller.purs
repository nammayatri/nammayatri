module Components.RideActionModal.Controller where

data Action = StartRide 
            | EndRide 
            | CancelRide 
            | OnNavigate 
            | CallCustomer 
            | LocationTracking
            | ButtonTimer Int String String String
            | NotifyCustomer

type Config = { 
  startRideActive :: Boolean,
  totalDistance :: String,
  customerName :: String,
  sourceAddress :: AddressConfig,
  destinationAddress :: AddressConfig,
  isDriverArrived :: Boolean,
  estimatedRideFare :: Int,
  notifiedCustomer :: Boolean,
  id :: String,
  buttonTimeOut :: Int
  }

type AddressConfig = {
  titleText :: String,
  detailText :: String
}

config :: Config
config = {
  startRideActive : false,
  totalDistance : "",
  customerName : "",
  sourceAddress : {
    titleText : "",
    detailText : ""
    },
  destinationAddress : {
  titleText : "",
  detailText : ""
  },
  isDriverArrived : true,
  estimatedRideFare : 0,
  notifiedCustomer : true,
  buttonTimeOut : 10,
  id : "buttonTimer"
}