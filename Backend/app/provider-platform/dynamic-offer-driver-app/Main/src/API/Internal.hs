module API.Internal
  ( API,
    handler,
  )
where

import qualified API.Internal.Auth as Auth
import qualified API.Internal.BulkLocUpdate as BulkLocUpdate
import qualified API.Internal.Cac as Cac
import qualified API.Internal.CallCustomerFCM as CallCustomerFCM
import qualified API.Internal.CustomerCancellationDues as CancellationDues
import qualified API.Internal.DriverCoordinates as DriverCoordinates
import qualified API.Internal.DriverInactiveFCM as DriverInactiveFCM
import qualified API.Internal.DriverReachedDestination as DriverReachedDestination
import qualified API.Internal.DriverReferee as DriverReferee
import qualified API.Internal.DriverSourceDeparted as DriverSourceDeparted
import qualified API.Internal.FavouriteDrivers as FavouriteDrivers
import qualified API.Internal.FeedbackForm as FeedbackForm
import qualified API.Internal.KnowYourDriver as KnowYourDriver
import qualified API.Internal.Multimodal as Multimodal
import qualified API.Internal.PopulateTipAmount as PopulateTipAmount
import qualified API.Internal.ProdLoopStatus as ProdLoopStatus
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
           :<|> CancellationDues.API
           :<|> DriverInactiveFCM.API
           :<|> CallCustomerFCM.API
           :<|> Cac.API
           :<|> FavouriteDrivers.API
           :<|> KnowYourDriver.API
           :<|> DriverCoordinates.API
           :<|> PopulateTipAmount.API
           :<|> Ride.API
           :<|> StopDetection.API
           :<|> Multimodal.API
           :<|> DriverReachedDestination.API
           :<|> ProdLoopStatus.API
           :<|> DriverSourceDeparted.API
           :<|> ViolationDetection.API
       )

handler :: FlowServer API
handler =
  DriverReferee.handler
    :<|> FeedbackForm.handler
    :<|> ReportACIssue.handler
    :<|> ReportIssue.handler
    :<|> Auth.handler
    :<|> BulkLocUpdate.handler
    :<|> CancellationDues.handler
    :<|> DriverInactiveFCM.handler
    :<|> CallCustomerFCM.handler
    :<|> Cac.handler
    :<|> FavouriteDrivers.handler
    :<|> KnowYourDriver.handler
    :<|> DriverCoordinates.handler
    :<|> PopulateTipAmount.handler
    :<|> Ride.handler
    :<|> StopDetection.handler
    :<|> Multimodal.handler
    :<|> DriverReachedDestination.handler
    :<|> ProdLoopStatus.handler
    :<|> DriverSourceDeparted.handler
    :<|> ViolationDetection.handler
