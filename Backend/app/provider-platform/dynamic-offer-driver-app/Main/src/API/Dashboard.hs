{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard where

import qualified API.Dashboard.Exotel as Exotel
import qualified API.Dashboard.Fleet as Fleet
import qualified API.Dashboard.Management as Management
import qualified API.Dashboard.RideBooking as RideBooking
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Servant hiding (throwError)

-- TODO :: Deprecated, Remove after successful deployment
type API =
  "dashboard"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> ( Management.API
           :<|> RideBooking.API
           :<|> Fleet.API
       )
    :<|> Exotel.API

type APIV2 =
  "dashboard"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> Capture "city" Context.City
    :> ( Management.API
           :<|> RideBooking.API
           :<|> Fleet.API
       )
    :<|> Exotel.API

-- TODO :: Deprecated, Remove after successful deployment
handler :: FlowServer API
handler =
  ( \merchantId -> do
      let city = getCity merchantId.getShortId
      Management.handler merchantId city
        :<|> RideBooking.handler merchantId city
        :<|> Fleet.handler merchantId city
  )
    :<|> Exotel.handler
  where
    getCity = \case
      -- this is temporary, will be removed after successful deployment
      "NAMMA_YATRI_PARTNER" -> Context.Bangalore
      "YATRI_PARTNER" -> Context.Kochi
      "JATRI_SAATHI_PARTNER" -> Context.Kolkata
      "PASSCULTURE_PARTNER" -> Context.Paris
      _ -> Context.AnyCity

handlerV2 :: FlowServer APIV2
handlerV2 =
  ( \merchantId city -> do
      Management.handler merchantId city
        :<|> RideBooking.handler merchantId city
        :<|> Fleet.handler merchantId city
  )
    :<|> Exotel.handler
