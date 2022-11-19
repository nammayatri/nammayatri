module API.Dashboard.Driver where

import qualified API.Dashboard.Driver.Registration as Reg
import Beckn.Types.Id
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver as Common
import Domain.Action.Dashboard.Driver
import qualified Domain.Types.Merchant as DM
import Environment
import Servant hiding (throwError)

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

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  driverDocumentsInfo merchantId
    :<|> listDrivers merchantId
    :<|> driverActivity merchantId
    :<|> enableDrivers merchantId
    :<|> disableDrivers merchantId
    :<|> driverLocation merchantId
    :<|> driverInfo merchantId
    :<|> deleteDriver merchantId
    :<|> Reg.handler merchantId
