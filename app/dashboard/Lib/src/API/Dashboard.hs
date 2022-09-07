module API.Dashboard where

import qualified API.Dashboard.Person as Person
import qualified API.Dashboard.Registration as Registration
import Environment
import Servant

type API =
  Person.API
    :<|> Registration.API

handler :: FlowServer API
handler =
  Person.handler
    :<|> Registration.handler
