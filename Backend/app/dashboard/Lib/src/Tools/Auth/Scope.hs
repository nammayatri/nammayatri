{-
 Merchant/city authorization for dashboard requests, run at HANDLER ENTRY.

 One token per person: the token no longer carries merchant/city. Merchant and
 city arrive as the handler's typed `Capture` args; the person arrives via the
 `DashboardUser` the pre-handler combinator produced. So the check is an ordinary
 Flow action the generated handler calls first, before the domain action.

 The merchant is compared by its short-id TEXT (not a typed `ShortId Merchant`)
 so this lib need not share the application server's `Merchant` phantom — the
 per-app wrapper (`enforceMerchantCityAccessForUser`) passes `getShortId`.
-}
module Tools.Auth.Scope
  ( resolveMerchantAccess,
    enforceMerchantCityAccess,
    invalidateMerchantAccess,
    merchantAccessCacheKey,
  )
where

import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error (AuthError (AccessDenied))
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.MerchantAccess as QAccess

-- | One Redis key per person holding their full merchant_access set, matched
-- directly (no merchant_pair hop). Mirrors Tools.Auth.Capability's person-keyed
-- cache + invalidation shape.
merchantAccessCacheKey :: Id DP.Person -> Text
merchantAccessCacheKey personId = "dsh:macc:" <> personId.getId

merchantAccessTtl :: Int
merchantAccessTtl = 600

-- | The person's (merchantShortId-as-Text, operatingCity) pairs, cached.
resolveMerchantAccess ::
  (BeamFlow m r, Redis.HedisFlow m r) =>
  Id DP.Person ->
  m [(Text, City.City)]
resolveMerchantAccess personId = do
  let key = merchantAccessCacheKey personId
  Redis.get key >>= \case
    Just cached -> pure cached
    Nothing -> do
      rows <- QAccess.findAllMerchantAccessByPersonId personId
      let pairs = [(getShortId row.merchantShortId, row.operatingCity) | row <- rows]
      Redis.setExp key pairs merchantAccessTtl
      pure pairs

-- | THE handler-entry check. @merchantShortId@ is the URL merchant's short id
-- (Text); @city@ is the URL city. Both come from the handler's typed captures.
enforceMerchantCityAccess ::
  (BeamFlow m r, Redis.HedisFlow m r) =>
  Id DP.Person ->
  Text ->
  City.City ->
  m ()
enforceMerchantCityAccess personId merchantShortId city = do
  accessSet <- resolveMerchantAccess personId
  unless (any (\(m, c) -> m == merchantShortId && c == city) accessSet) $
    throwError AccessDenied

-- | Drop the cached set on any merchant_access change / person disable.
invalidateMerchantAccess :: Redis.HedisFlow m r => Id DP.Person -> m ()
invalidateMerchantAccess personId = Redis.del (merchantAccessCacheKey personId)
