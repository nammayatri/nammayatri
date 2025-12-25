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
    Nothing -> flip whenJust (cacheBapMetadata subscriberId domain) /=<< Queries.findBySubscriberIdAndDomain subscriberId (Just domain)

cacheBapMetadata :: (CacheFlow m r) => Id BapMetadata -> Domain -> BapMetadata -> m ()
cacheBapMetadata subscriberId domain bapMetadata = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeSubscriberIdKey subscriberId domain
  Hedis.setExp idKey bapMetadata expTime

makeSubscriberIdKey :: Id BapMetadata -> Text -> Text
makeSubscriberIdKey subscriberId domain = "CachedQueries:BapMetadata:" <> domain <> ":sid-" <> subscriberId.getId
