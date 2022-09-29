module API.Dashboard where

import qualified API.Dashboard.AccessMatrix as AccessMatrix
import qualified API.Dashboard.Person as Person
import qualified API.Dashboard.Registration as Registration
import qualified API.Dashboard.Roles as Roles
import Environment
import Servant

type API =
  "dashboard"
    :> ( Person.API
           :<|> Registration.API
           :<|> AccessMatrix.API
           :<|> Roles.API
       )

handler :: FlowServer API
handler =
  Person.handler
    :<|> Registration.handler
    :<|> AccessMatrix.handler
    :<|> Roles.handler
