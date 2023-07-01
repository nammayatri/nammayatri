module API.Internal
  ( API,
    handler,
  )
where

import qualified API.Internal.DriverReferee as DriverReferee
import qualified API.Internal.FeedbackForm as FeedbackForm
import Environment
import Servant

type API =
  "internal"
    :> ( DriverReferee.API
           :<|> FeedbackForm.API
       )

handler :: FlowServer API
handler =
  DriverReferee.handler
    :<|> FeedbackForm.handler
