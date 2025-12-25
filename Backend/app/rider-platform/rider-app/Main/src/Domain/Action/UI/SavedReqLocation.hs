{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.SavedReqLocation
  ( CreateSavedReqLocationReq (..),
    SavedReqLocationsListRes (..),
    createSavedReqLocation,
    getSavedReqLocations,
    deleteSavedReqLocation,
  )
where

import Data.Text (pack)
import qualified Domain.Types.Person as Person
import Domain.Types.SavedReqLocation
import qualified Domain.Types.SavedReqLocation as SavedReqLocation
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Error
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import qualified Storage.Queries.SavedReqLocation as QSavedReqLocation

makeSavedReqLocationAPIEntity :: SavedReqLocation -> SavedReqLocationAPIEntity
makeSavedReqLocationAPIEntity SavedReqLocation {..} =
  SavedReqLocationAPIEntity
    { ..
    }

data CreateSavedReqLocationReq = CreateSavedReqLocationReq
  { lat :: Double,
    lon :: Double,
    street :: Maybe Text,
    door :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text,
    tag :: Text,
    placeId :: Maybe Text,
    ward :: Maybe Text,
    isMoved :: Maybe Bool,
    locationName :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype SavedReqLocationsListRes = SavedReqLocationsListRes
  { list :: [SavedReqLocation.SavedReqLocationAPIEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

createSavedReqLocation :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person.Person -> CreateSavedReqLocationReq -> m APISuccess.APISuccess
createSavedReqLocation riderId sreq = do
  savedLocations <- QSavedReqLocation.findAllByRiderIdAndTag riderId sreq.tag
  when (sreq.tag == pack "") $ throwError $ InvalidRequest "Location tag cannot be empty"
  unless (null savedLocations) $ throwError $ InvalidRequest "Location with this tag already exists"
  now <- getCurrentTime
  location <- buildSavedReqLocation sreq now riderId
  _ <- QSavedReqLocation.create location
  return APISuccess.Success

getSavedReqLocations :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Person.Person -> m SavedReqLocationsListRes
getSavedReqLocations riderId = do
  savedLocations <- runInReplica $ QSavedReqLocation.findAllByRiderId riderId
  return $ SavedReqLocationsListRes $ makeSavedReqLocationAPIEntity <$> savedLocations

deleteSavedReqLocation :: EsqDBFlow m r => Id Person.Person -> Text -> m APISuccess.APISuccess
deleteSavedReqLocation riderId tag = do
  void $ QSavedReqLocation.deleteByRiderIdAndTag riderId tag
  return APISuccess.Success

buildSavedReqLocation :: MonadFlow m => CreateSavedReqLocationReq -> UTCTime -> Id Person.Person -> m SavedReqLocation.SavedReqLocation
buildSavedReqLocation CreateSavedReqLocationReq {..} now riderId = do
  locId <- generateGUID
  return
    SavedReqLocation.SavedReqLocation
      { id = locId,
        createdAt = now,
        updatedAt = now,
        ..
      }
