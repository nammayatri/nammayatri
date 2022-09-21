module API.BAP
  ( API,
    handler,
  )
where

import qualified API.BAP.ARDU as ARDU
import qualified API.BAP.Yatri as Yatri
import "lib-dashboard" Environment
import Servant

type API =
  "bap"
    :> ( ARDU.API
           :<|> Yatri.API
       )

handler :: FlowServer API
handler =
  ARDU.handler
    :<|> Yatri.handler
