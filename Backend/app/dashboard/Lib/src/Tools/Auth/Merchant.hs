{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tools.Auth.Merchant (merchantCityAccessCheck, merchantServerAccessCheck, CheckedShortId, skipMerchantCityAccessCheck) where

import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.ServerName as DTServer
import Kernel.Prelude
import Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant (ToHttpApiData)
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantPair as QMerchantPair

-- | Authorize a request whose URL names a merchant against the merchant the
-- token is bound to.
--
-- Arguments, in call order: the merchant/city from the URL, then the merchant/
-- city from the token.
--
-- Dashboard unification: BAP routes are served by the provider server, so a
-- BPP token legitimately addresses BAP-tree URLs — /bpp/bap/JATRI_SAATHI/...
-- with a token bound to JATRI_SAATHI_PARTNER. A plain equality check denies
-- every one of those. `Tools.Auth.Api.verifyServerWithPair` already resolves
-- the logical partner for the SERVER check; this is the same idea for the
-- merchant short id.
--
-- Note it does not always fall through to the pair: verifyServerWithPair only
-- hops when the token's merchant cannot serve the required server, and the BPP
-- merchants list the BAP server names too — so auth succeeds holding the BPP
-- merchant and lands here with a mismatch.
--
-- The URL's short id is what gets returned and forwarded downstream, because
-- that is the one the target platform knows (rider-app expects the BAP short
-- id). The old code returned the token's, which was indistinguishable while
-- the two were required to be equal.
--
-- The pair lookup only runs when the short ids differ, so the common case
-- costs no extra queries.
merchantCityAccessCheck ::
  BeamFlow m r =>
  ShortId DMerchant.Merchant ->
  ShortId DMerchant.Merchant ->
  City.City ->
  City.City ->
  m (CheckedShortId DMerchant.Merchant)
merchantCityAccessCheck urlMerchantShortId tokenMerchantShortId urlCity tokenCity = do
  unless (urlCity == tokenCity) $ throwError AccessDenied
  let ShortId urlMerchantId = urlMerchantShortId
      ShortId tokenMerchantId = tokenMerchantShortId
  unless (urlMerchantId == tokenMerchantId) $
    unlessM (isLogicalPair urlMerchantShortId tokenMerchantShortId) $ throwError AccessDenied
  pure $ CheckedShortId urlMerchantId

-- | Are these two short ids the two sides of one merchant_pair row?
isLogicalPair ::
  BeamFlow m r =>
  ShortId DMerchant.Merchant ->
  ShortId DMerchant.Merchant ->
  m Bool
isLogicalPair urlMerchantShortId tokenMerchantShortId = do
  mbUrlMerchant <- QMerchant.findByShortId urlMerchantShortId
  mbTokenMerchant <- QMerchant.findByShortId tokenMerchantShortId
  case (mbUrlMerchant, mbTokenMerchant) of
    (Just urlMerchant, Just tokenMerchant) -> do
      mbPair <- QMerchantPair.findByMerchantId tokenMerchant.id
      pure $ case mbPair of
        Nothing -> False
        Just pair ->
          let sides = catMaybes [pair.bapMerchantId, pair.bppMerchantId]
           in urlMerchant.id `elem` sides && tokenMerchant.id `elem` sides
    _ -> pure False

skipMerchantCityAccessCheck :: ShortId DMerchant.Merchant -> CheckedShortId DMerchant.Merchant
skipMerchantCityAccessCheck (ShortId merchantId) = CheckedShortId merchantId

-- CheckedShortId constructor should not be exported for type safety
newtype CheckedShortId domain = CheckedShortId Text
  deriving newtype (ToHttpApiData, ToSchema)

merchantServerAccessCheck ::
  ( MonadFlow m,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]]
  ) =>
  DMerchant.Merchant ->
  m ()
merchantServerAccessCheck merchant = do
  availableServers <- asks (.dataServers)
  unless (all (`elem` (availableServers <&> (.name))) merchant.serverNames) $
    throwError $ InvalidRequest "Server for this merchant is not available"
