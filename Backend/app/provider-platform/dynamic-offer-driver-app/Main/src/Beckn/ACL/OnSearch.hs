{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnSearch where

import qualified Beckn.OnDemand.Transformer.OnSearch as TOnSearch
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils (computeTtlISO8601)
import Control.Applicative ((<|>))
import qualified Domain.Action.Beckn.Search as DSearch
import Kernel.Prelude
import Kernel.Types.App
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Storage.CachedQueries.BecknConfig as QBC

mkOnSearchRequest ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  DSearch.DSearchRes ->
  Context.Action ->
  Context.Domain ->
  Text ->
  Maybe Text ->
  BaseUrl ->
  Maybe Text ->
  Maybe BaseUrl ->
  Context.City ->
  Context.Country ->
  Bool ->
  m Spec.OnSearchReq
mkOnSearchRequest res@DSearch.DSearchRes {..} action domain messageId transactionId bapUri bppId bppUri city country isValueAddNP = do
  bppConfig <- QBC.findByMerchantIdDomainAndVehicle provider.id "MOBILITY" Enums.AUTO_RICKSHAW >>= fromMaybeM (InternalError $ "Beckn Config not found for merchantId:-" <> show provider.id.getId <> ",domain:-MOBILITY,vehicleVariant:-" <> show Enums.AUTO_RICKSHAW)
  ttl <- (if fromMaybe False isMultimodalSearch then bppConfig.multimodalOnSearchTTLSec <|> bppConfig.onSearchTTLSec else bppConfig.onSearchTTLSec) & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  TOnSearch.buildOnSearchRideReq ttl bppConfig res action domain messageId transactionId bapId bapUri bppId bppUri city country isValueAddNP
