{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Storage.CachedQueries.BapMetadata where

import Domain.Types.BapMetadata
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.BapMetadata as Queries

type Domain = Text

createIfNotPresent :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => BapMetadata -> Id BapMetadata -> Domain -> m ()
createIfNotPresent bapMetadata subscriberId domain = do
  maybeBapMetadata <- findBySubscriberIdAndDomain' subscriberId domain
  whenNothing maybeBapMetadata $ do
    void $ Queries.create bapMetadata
    void $ cacheBapMetadata subscriberId domain bapMetadata
    -- overwrite any Nothing cached for this city by a lookup made earlier in the same request
    whenJust bapMetadata.merchantOperatingCityId $ \merchantOpCityId -> cacheBapMetadataForCity subscriberId domain merchantOpCityId (Just bapMetadata)
  where
    whenNothing m = when (isNothing m)

findBySubscriberIdAndDomain :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id BapMetadata -> Context.Domain -> m (Maybe BapMetadata)
findBySubscriberIdAndDomain subscriberId domain = do
  let domainText = show domain
  findBySubscriberIdAndDomain' subscriberId domainText

findBySubscriberIdAndDomain' :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id BapMetadata -> Domain -> m (Maybe BapMetadata)
findBySubscriberIdAndDomain' subscriberId domain =
  Hedis.safeGet (makeSubscriberIdKey subscriberId domain) >>= \case
    Just a -> return $ Just a
    Nothing -> flip whenJust (cacheBapMetadata subscriberId domain) /=<< (listToMaybe <$> Queries.findAllBySubscriberIdAndDomain subscriberId (Just domain))

-- | ONDC 2.1.0 scheduled-ride pilot lookup: the BAP's row only if it is tagged with this
-- operating city, so a BPP city can run 2.1.0 for some BAPs and the old flow for the rest.
-- Nothing is cached too, since most BAPs aren't piloting and this is read on every Beckn message.
findBySubscriberIdDomainAndCity :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id BapMetadata -> Context.Domain -> Id DMOC.MerchantOperatingCity -> m (Maybe BapMetadata)
findBySubscriberIdDomainAndCity subscriberId domain merchantOpCityId = do
  let domainText = show domain
  Hedis.safeGet (makeSubscriberIdAndCityKey subscriberId domainText merchantOpCityId) >>= \case
    Just a -> return a
    Nothing -> do
      mbBapMetadata <- Queries.findBySubscriberIdDomainAndCity subscriberId (Just domainText) (Just merchantOpCityId)
      cacheBapMetadataForCity subscriberId domainText merchantOpCityId mbBapMetadata
      return mbBapMetadata

isOndcScheduledRideSupportEnabled :: Maybe BapMetadata -> Bool
isOndcScheduledRideSupportEnabled mbBapMetadata = fromMaybe False (mbBapMetadata >>= (.enableOndcScheduledRideSupport))

-- | ONDC scheduled-ride pilot: store the BAP's own declared BAP_TERMS.STATIC_TERMS URL (parsed at
-- /search, /init, /confirm) against its BapMetadata row for this operating city. Update-only, no
-- insert -- if no row exists yet for this subscriber+domain+city (e.g. this is the
-- very first request ever seen from a brand-new BAP, before Layer 1's own
-- createIfNotPresent has had a chance to run), this is a deliberate no-op: it
-- doesn't fabricate a row with placeholder name/logo data, it just waits for
-- the next request from the same BAP, by which point Layer 1 will have
-- created the row properly.
updateStaticTermsUrlIfChanged :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id BapMetadata -> Domain -> Id DMOC.MerchantOperatingCity -> Maybe BapMetadata -> BaseUrl -> m (Maybe BapMetadata)
updateStaticTermsUrlIfChanged subscriberId domain merchantOpCityId mbExisting newStaticTermsUrl =
  case mbExisting of
    Just existing | existing.staticTermsUrl /= Just newStaticTermsUrl -> do
      let updated = existing {staticTermsUrl = Just newStaticTermsUrl}
      Queries.updateStaticTermsUrlBySubscriberIdDomainAndCity (Just newStaticTermsUrl) subscriberId (Just domain) (Just merchantOpCityId)
      cacheBapMetadata subscriberId domain updated
      cacheBapMetadataForCity subscriberId domain merchantOpCityId (Just updated)
      pure $ Just updated
    _ -> pure mbExisting

cacheBapMetadata :: (CacheFlow m r) => Id BapMetadata -> Domain -> BapMetadata -> m ()
cacheBapMetadata subscriberId domain bapMetadata = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeSubscriberIdKey subscriberId domain
  Hedis.setExp idKey bapMetadata expTime

cacheBapMetadataForCity :: (CacheFlow m r) => Id BapMetadata -> Domain -> Id DMOC.MerchantOperatingCity -> Maybe BapMetadata -> m ()
cacheBapMetadataForCity subscriberId domain merchantOpCityId mbBapMetadata = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeSubscriberIdAndCityKey subscriberId domain merchantOpCityId) mbBapMetadata expTime

makeSubscriberIdKey :: Id BapMetadata -> Text -> Text
makeSubscriberIdKey subscriberId domain = "CachedQueries:BapMetadata:" <> domain <> ":sid-" <> subscriberId.getId

makeSubscriberIdAndCityKey :: Id BapMetadata -> Text -> Id DMOC.MerchantOperatingCity -> Text
makeSubscriberIdAndCityKey subscriberId domain merchantOpCityId = makeSubscriberIdKey subscriberId domain <> ":mocid-" <> merchantOpCityId.getId
