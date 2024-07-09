module API.Internal
  ( API,
    handler,
  )
where

import qualified API.Internal.Auth as Auth
import qualified API.Internal.BulkLocUpdate as BulkLocUpdate
import qualified API.Internal.Cac as Cac
import qualified API.Internal.CustomerCancellationDues as CancellationDues
import qualified API.Internal.DriverInactiveFCM as DriverInactiveFCM
import qualified API.Internal.DriverReferee as DriverReferee
import qualified API.Internal.FavouriteDrivers as FavouriteDrivers
import qualified API.Internal.FeedbackForm as FeedbackForm
import qualified API.Internal.KnowYourDriver as KnowYourDriver
import qualified API.Internal.ReportACIssue as ReportACIssue
import qualified API.Internal.ReportIssue as ReportIssue
import qualified API.Internal.VehicleService as VehicleService
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
           :<|> Cac.API
           :<|> FavouriteDrivers.API
           :<|> KnowYourDriver.API
           :<|> VehicleService.API
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
    :<|> Cac.handler
    :<|> FavouriteDrivers.handler
    :<|> KnowYourDriver.handler
    :<|> VehicleService.handler
