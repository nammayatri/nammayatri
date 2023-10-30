{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tools.Auth.Merchant (merchantCityAccessCheck, CheckedShortId) where

import qualified Domain.Types.Merchant as DMerchant
import Kernel.Prelude
import Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant (ToHttpApiData)

merchantCityAccessCheck :: MonadFlow m => ShortId DMerchant.Merchant -> ShortId DMerchant.Merchant -> City.City -> City.City -> m (CheckedShortId DMerchant.Merchant)
merchantCityAccessCheck (ShortId userMerchantId) (ShortId merchantId) userCity city = do
  unless (userMerchantId == merchantId && userCity == city) $ throwError AccessDenied
  pure $ CheckedShortId merchantId

-- CheckedShortId constructor should not be exported for type safety
newtype CheckedShortId domain = CheckedShortId Text
  deriving newtype (ToHttpApiData, ToSchema)
