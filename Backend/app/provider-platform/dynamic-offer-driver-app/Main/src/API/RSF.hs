module API.RSF (API, handler) where

import qualified API.RSF.ReceiverRecon as ReceiverRecon
import Environment

type API =
  ReceiverRecon.API

handler :: FlowServer API
handler =
  ReceiverRecon.handler
