module API.Internal
  ( API,
    handler,
  )
where

import qualified API.Internal.Auth as Auth
import qualified API.Internal.BulkLocUpdate as BulkLocUpdate
import qualified API.Internal.Cac as Cac
import qualified API.Internal.CallCustomerFCM as CallCustomerFCM
import qualified API.Internal.CancellationDues as CancellationDues
import qualified API.Internal.CustomerCancellationDues as CustomerCancellationDues
import qualified API.Internal.DriverCoordinates as DriverCoordinates
import qualified API.Internal.DriverInactiveFCM as DriverInactiveFCM
import qualified API.Internal.DriverReachedDestination as DriverReachedDestination
import qualified API.Internal.DriverReferee as DriverReferee
import qualified API.Internal.DriverSourceDeparted as DriverSourceDeparted
import qualified API.Internal.Estimate as Estimate
import qualified API.Internal.FavouriteDrivers as FavouriteDrivers
import qualified API.Internal.FeedbackForm as FeedbackForm
import qualified API.Internal.FleetBookingInformation as FleetBookingInformation
import qualified API.Internal.FleetVehiclesAssociation as FleetVehiclesAssociation
import qualified API.Internal.KnowYourDriver as KnowYourDriver
import qualified API.Internal.Multimodal as Multimodal
import qualified API.Internal.PickupInstruction as PickupInstruction
import qualified API.Internal.PopulateTipAmount as PopulateTipAmount
import qualified API.Internal.ProdLoopStatus as ProdLoopStatus
import qualified API.Internal.QuoteRespond as QuoteRespond
import qualified API.Internal.ReportACIssue as ReportACIssue
import qualified API.Internal.ReportIssue as ReportIssue
import qualified API.Internal.Ride as Ride
import qualified API.Internal.StopDetection as StopDetection
import qualified API.Internal.ViolationDetection as ViolationDetection
import Environment
import Servant
import Tools.Auth ()

type API =
  "internal"
    :> ( DriverReferee.API
           :<|> FeedbackForm.API
           :<|> ReportACIssue.API -- Deprecated
           :<|> ReportIssue.API
           :<|> Auth.API
           :<|> BulkLocUpdate.API
           :<|> CustomerCancellationDues.API
           :<|> DriverInactiveFCM.API
           :<|> CallCustomerFCM.API
           :<|> Cac.API
           :<|> FavouriteDrivers.API
           :<|> KnowYourDriver.API
           :<|> DriverCoordinates.API
           :<|> PickupInstruction.API
           :<|> PopulateTipAmount.API
           :<|> Ride.API
           :<|> StopDetection.API
           :<|> Multimodal.API
           :<|> DriverReachedDestination.API
           :<|> QuoteRespond.API
           :<|> ProdLoopStatus.API
           :<|> DriverSourceDeparted.API
           :<|> ViolationDetection.API
           :<|> Estimate.API
           :<|> FleetBookingInformation.API
           :<|> FleetVehiclesAssociation.API
           :<|> CancellationDues.CustomerCancellationDuesWaiveOffAPI
       )

handler :: FlowServer API
handler =
  DriverReferee.handler
    :<|> FeedbackForm.handler
    :<|> ReportACIssue.handler
    :<|> ReportIssue.handler
    :<|> Auth.handler
    :<|> BulkLocUpdate.handler
    :<|> CustomerCancellationDues.handler
    :<|> DriverInactiveFCM.handler
    :<|> CallCustomerFCM.handler
    :<|> Cac.handler
    :<|> FavouriteDrivers.handler
    :<|> KnowYourDriver.handler
    :<|> DriverCoordinates.handler
    :<|> PickupInstruction.handler
    :<|> PopulateTipAmount.handler
    :<|> Ride.handler
    :<|> StopDetection.handler
    :<|> Multimodal.handler
    :<|> DriverReachedDestination.handler
    :<|> QuoteRespond.handler
    :<|> ProdLoopStatus.handler
    :<|> DriverSourceDeparted.handler
    :<|> ViolationDetection.handler
    :<|> Estimate.handler
    :<|> FleetBookingInformation.handler
    :<|> FleetVehiclesAssociation.handler
    :<|> CancellationDues.handler
