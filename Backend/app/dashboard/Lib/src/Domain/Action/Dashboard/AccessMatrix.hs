{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.AccessMatrix where

import qualified Data.Text as T
import qualified Domain.Types.MerchantCityList as DMatrix
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Utils.Common (logInfo)
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Merchant as QMerchant

getMerchantWithCityList ::
  BeamFlow m r =>
  m [DMatrix.MerchantCityList]
getMerchantWithCityList = do
  logInfo "[AccessMatrix.getMerchantWithCityList] START"
  merchantList <- B.runInReplica QMerchant.findAllMerchants
  logInfo $ "[AccessMatrix.getMerchantWithCityList] findAllMerchants done, count=" <> show (length merchantList)
  let merchantCityList = map (\merchant -> DMatrix.MerchantCityList merchant.shortId merchant.supportedOperatingCities (map (T.pack . show) merchant.supportedOperatingCities)) merchantList
  pure merchantCityList
