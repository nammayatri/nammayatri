module API.Internal
  ( API,
    handler,
  )
where

import qualified API.Internal.Cac as Cac
import qualified API.Internal.DriverArrivalNotf as DriverArrivalNotf
import qualified API.Internal.FRFS as FRFS
import qualified API.Internal.FrequentLocUser as FrequentLocUser
import qualified API.Internal.Rating as Rating
import qualified API.Internal.StopEvents as StopEvents
import Environment
import Servant
import Tools.Auth ()

type API =
  "internal"
    :> ( Rating.API
           :<|> FRFS.API
           :<|> Cac.API
           :<|> StopEvents.API
           :<|> FrequentLocUser.API
           :<|> DriverArrivalNotf.API
       )

handler :: FlowServer API
handler =
  Rating.handler
    :<|> FRFS.handler
    :<|> Cac.handler
    :<|> StopEvents.handler
    :<|> FrequentLocUser.handler
    :<|> DriverArrivalNotf.handler
