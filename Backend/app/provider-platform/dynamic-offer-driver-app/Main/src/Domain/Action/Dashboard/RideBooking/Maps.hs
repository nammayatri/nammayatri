{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.RideBooking.Maps
  ( postMapsAutoComplete,
    postMapsGetPlaceName,
  )
where

import qualified Domain.Action.UI.Maps as DMaps
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as City
import Kernel.Types.Id
import Kernel.Utils.Logging
import SharedLogic.Merchant
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

postMapsAutoComplete :: ShortId DM.Merchant -> City.City -> Id DP.Person -> DMaps.AutoCompleteReq -> Flow DMaps.AutoCompleteResp
postMapsAutoComplete merchantId city personId req = withPersonIdLogTag personId $ do
  m <- findMerchantByShortId merchantId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing m (Just city)
  withPersonIdLogTag personId $
    DMaps.autoComplete m.id merchantOpCityId (Just personId.getId) req

postMapsGetPlaceName :: ShortId DM.Merchant -> City.City -> Id DP.Person -> DMaps.GetPlaceNameReq -> Flow DMaps.GetPlaceNameResp
postMapsGetPlaceName merchantId city personId req = withPersonIdLogTag personId $ do
  m <- findMerchantByShortId merchantId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing m (Just city)
  withPersonIdLogTag personId $
    DMaps.getPlaceName m.id merchantOpCityId (Just personId.getId) req
