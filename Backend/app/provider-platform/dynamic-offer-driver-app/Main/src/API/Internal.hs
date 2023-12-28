module API.Internal
  ( API,
    handler,
  )
where

import qualified API.Internal.Auth as Auth
import qualified API.Internal.BulkLocUpdate as BulkLocUpdate
import qualified API.Internal.CustomerCancellationDues as CancellationDues
import qualified API.Internal.DriverReferee as DriverReferee
import qualified API.Internal.FeedbackForm as FeedbackForm
import Environment
import Servant
import Tools.Auth ()

type API =
  "internal"
    :> ( DriverReferee.API
           :<|> FeedbackForm.API
           :<|> Auth.API
           :<|> BulkLocUpdate.API
           :<|> CancellationDues.API
       )

handler :: FlowServer API
handler =
  DriverReferee.handler
    :<|> FeedbackForm.handler
    :<|> Auth.handler
    :<|> BulkLocUpdate.handler
    :<|> CancellationDues.handler
