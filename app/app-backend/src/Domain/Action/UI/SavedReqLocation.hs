module Domain.Action.UI.SavedReqLocation
  ( CreateSavedReqLocationReq (..),
    SavedReqLocationsListRes (..),
    createSavedReqLocation,
    getSavedReqLocations,
    deleteSavedReqLocation,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Beckn.Storage.Esqueleto.Transactionable (runTransaction)
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Error
import Beckn.Types.Id (Id)
import Beckn.Utils.Common
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SavedReqLocation as SavedReqLocation
import qualified Storage.Queries.SavedReqLocation as QSavedReqLocation

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
    tag :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype SavedReqLocationsListRes = SavedReqLocationsListRes
  { list :: [SavedReqLocation.SavedReqLocationAPIEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

createSavedReqLocation :: (EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Person.Person -> CreateSavedReqLocationReq -> m APISuccess.APISuccess
createSavedReqLocation riderId sreq = do
  savedLocations <- QSavedReqLocation.findAllByRiderIdAndTag riderId sreq.tag
  unless (null savedLocations) $ throwError $ InvalidRequest "Location with this tag already exists"
  now <- getCurrentTime
  location <- buildSavedReqLocation sreq now riderId
  runTransaction $
    QSavedReqLocation.create location
  return APISuccess.Success

getSavedReqLocations :: EsqDBReplicaFlow m r => Id Person.Person -> m SavedReqLocationsListRes
getSavedReqLocations riderId = do
  savedLocations <- QSavedReqLocation.findAllByRiderId riderId
  return $ SavedReqLocationsListRes $ SavedReqLocation.makeSavedReqLocationAPIEntity <$> savedLocations

deleteSavedReqLocation :: EsqDBFlow m r => Id Person.Person -> Text -> m APISuccess.APISuccess
deleteSavedReqLocation riderId tag = do
  runTransaction $
    QSavedReqLocation.deleteByRiderIdAndTag riderId tag
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
