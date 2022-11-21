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
    :> ( Common.DriverDocumentsInfoAPI
           :<|> Common.DriverListAPI
           :<|> Common.DriverActivityAPI
           :<|> Common.EnableDriversAPI
           :<|> Common.DisableDriversAPI
           :<|> Common.DriverLocationAPI
           :<|> Common.DriverInfoAPI
           :<|> Common.DeleteDriverAPI
           :<|> Reg.API
       )

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
