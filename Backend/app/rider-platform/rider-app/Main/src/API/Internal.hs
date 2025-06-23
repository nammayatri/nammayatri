module API.Internal
  ( API,
    handler,
  )
where

import qualified API.Action.UI.InsuranceInternal as InsuranceInternal
import qualified API.Action.UI.MeterRideInternal as MeterRideInternal
import qualified API.Internal.Cac as Cac
import qualified API.Internal.DriverArrivalNotf as DriverArrivalNotf
import qualified API.Internal.FRFS as FRFS
import qualified API.Internal.FrequentLocUser as FrequentLocUser
import qualified API.Internal.Rating as Rating
import qualified API.Internal.RideSearchExpired as RideSearchExpired
import qualified API.Internal.StopEvents as StopEvents
import qualified API.Internal.ViolationDetection as ViolationDetection
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
           :<|> MeterRideInternal.API
           :<|> InsuranceInternal.API
           :<|> ViolationDetection.API
           :<|> RideSearchExpired.API
       )

handler :: FlowServer API
handler =
  Rating.handler
    :<|> FRFS.handler
    :<|> Cac.handler
    :<|> StopEvents.handler
    :<|> FrequentLocUser.handler
    :<|> DriverArrivalNotf.handler
    :<|> MeterRideInternal.handler
    :<|> InsuranceInternal.handler
    :<|> ViolationDetection.handler
    :<|> RideSearchExpired.handler
