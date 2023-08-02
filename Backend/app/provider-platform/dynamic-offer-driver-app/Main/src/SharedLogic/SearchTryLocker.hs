{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.SearchTryLocker
  ( whenSearchTryCancellable,
    isSearchTryCancelled,
    markSearchTryAsAssigned,
  )
where

import Domain.Types.SearchTry (SearchTry)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)

isSearchTryCancelled ::
  CacheFlow m r =>
  Id SearchTry ->
  m Bool
isSearchTryCancelled searchTryId = do
  fromMaybe False <$> Hedis.get (mkCancelledKey searchTryId)

isSearchTryAssigned ::
  CacheFlow m r =>
  Id SearchTry ->
  m Bool
isSearchTryAssigned searchTryId = do
  fromMaybe False <$> Hedis.get (mkAssignedKey searchTryId)

whenSearchTryCancellable ::
  CacheFlow m r =>
  Id SearchTry ->
  m () ->
  m ()
whenSearchTryCancellable searchTryId actions = do
  isSearchTryCancelled' <- isSearchTryCancelled searchTryId
  isSearchTryAssigned' <- isSearchTryAssigned searchTryId
  unless (isSearchTryCancelled' || isSearchTryAssigned') $ do
    Hedis.setExp (mkCancelledKey searchTryId) True 120
    actions

markSearchTryAsAssigned ::
  CacheFlow m r =>
  Id SearchTry ->
  m ()
markSearchTryAsAssigned searchTryId = do
  Hedis.setExp (mkAssignedKey searchTryId) True 120

mkCancelledKey :: Id SearchTry -> Text
mkCancelledKey searchTryId = "SearchTry:Cancelled:SearchTryId-" <> searchTryId.getId

mkAssignedKey :: Id SearchTry -> Text
mkAssignedKey searchTryId = "SearchTry:Assigned:SearchTryId-" <> searchTryId.getId
