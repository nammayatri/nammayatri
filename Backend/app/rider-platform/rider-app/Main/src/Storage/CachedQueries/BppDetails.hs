{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.BppDetails
  ( createIfNotPresent,
    findBySubscriberIdAndDomain,
  )
where

import Domain.Types.BppDetails
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common
import qualified Storage.Queries.BppDetails as Queries

type Domain = Text

type SubscriberId = Text

createIfNotPresent :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => BppDetails -> m ()
createIfNotPresent bppDetails = do
  maybeBppDetails <- findBySubscriberIdAndDomain' bppDetails.subscriberId bppDetails.domain
  whenNothing maybeBppDetails $ do
    void $ Queries.create bppDetails
    void $ cacheBppDetails bppDetails
  where
    whenNothing m f = when (isNothing m) f

findBySubscriberIdAndDomain :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => SubscriberId -> Context.Domain -> m (Maybe BppDetails)
findBySubscriberIdAndDomain subscriberId domain = do
  let domainText = show domain
  findBySubscriberIdAndDomain' subscriberId domainText

findBySubscriberIdAndDomain' :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => SubscriberId -> Domain -> m (Maybe BppDetails)
findBySubscriberIdAndDomain' subscriberId domain =
  Hedis.safeGet (makeSubscriberIdKey subscriberId domain) >>= \case
    Just a -> return $ Just a
    Nothing -> flip whenJust cacheBppDetails /=<< Queries.findBySubscriberIdAndDomain subscriberId domain

cacheBppDetails :: (CacheFlow m r) => BppDetails -> m ()
cacheBppDetails bppDetails = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeSubscriberIdKey bppDetails.subscriberId bppDetails.domain
  Hedis.setExp idKey bppDetails expTime

makeSubscriberIdKey :: Text -> Text -> Text
makeSubscriberIdKey subscriberId domain = "CachedQueries:BppDetails:" <> domain <> ": sid-" <> subscriberId
