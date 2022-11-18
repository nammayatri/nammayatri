module API.Dashboard.Driver where

import qualified API.Dashboard.Driver.Registration as Reg
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver as Common
import Domain.Action.Dashboard.Driver
import Environment
import Servant hiding (throwError)
import Tools.Auth

type API =
  "driver"
    :> ( DriverDocumentsInfoAPI
           :<|> DriverListAPI
           :<|> DriverActivityAPI
           :<|> EnableDriversAPI
           :<|> DisableDriversAPI
           :<|> DriverLocationAPI
           :<|> DriverInfoAPI
           :<|> DeleteDriverAPI
           :<|> Reg.API
       )

type DriverDocumentsInfoAPI =
  "documents"
    :> "info"
    :> Common.DriverDocumentsInfoAPI

-- TODO refactor it so that "list" becomes part of Common.DriverListAPI
type DriverListAPI = "list" :> Common.DriverListAPI

type DriverActivityAPI = "activity" :> Common.DriverActivityAPI

type EnableDriversAPI = "enable" :> Common.EnableDriversAPI

type DisableDriversAPI = "disable" :> Common.DisableDriversAPI

type DriverLocationAPI = "location" :> Common.DriverLocationAPI

type DriverInfoAPI = "info" :> Common.DriverInfoAPI

type DeleteDriverAPI = Common.DeleteDriverAPI

handler :: Dashboard -> FlowServer API
handler d =
  driverDocumentsInfo
    :<|> listDrivers
    :<|> driverActivity
    :<|> enableDrivers
    :<|> disableDrivers
    :<|> driverLocation
    :<|> driverInfo
    :<|> deleteDriver
    :<|> Reg.handler d
