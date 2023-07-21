module API.Internal
  ( API,
    handler,
  )
where

import qualified API.Internal.Auth as Auth
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
       )

handler :: FlowServer API
handler =
  DriverReferee.handler
    :<|> FeedbackForm.handler
    :<|> Auth.handler
