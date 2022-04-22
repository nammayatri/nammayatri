module Product.SavedLocations where

import App.Types (FlowHandler)
import Beckn.Storage.Esqueleto.Transactionable (runTransaction)
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Error
import Beckn.Types.Id (Id)
import Beckn.Utils.Common
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SavedReqLocation as SavedReqLocation
import qualified Domain.Types.SearchReqLocation as SearchLocation
import EulerHS.Prelude hiding (state)
import qualified Storage.Queries.SavedReqLocation as QSavedReqLocation
import qualified Types.API.SavedLocations as SavedLocation

createSavedLocationEntity :: Id Person.Person -> SavedLocation.SavedReqLocationAPIEntity -> FlowHandler APISuccess.APISuccess
createSavedLocationEntity riderId sreq = withFlowHandlerAPI . withPersonIdLogTag riderId $ do
  savedLocations <- QSavedReqLocation.findAllByRiderIdAndTag riderId sreq.tag
  unless (null savedLocations) $ throwError $ InvalidRequest "Location with this tag already exists"
  now <- getCurrentTime
  location <- buildSavedReqLocationAPIEntity sreq now riderId
  runTransaction $
    QSavedReqLocation.create location
  return APISuccess.Success

getSavedLocations :: Id Person.Person -> FlowHandler SavedLocation.SavedLocationsListRes
getSavedLocations riderId = withFlowHandlerAPI . withPersonIdLogTag riderId $ do
  savedLocations <- QSavedReqLocation.findAllByRiderId riderId
  return $ SavedLocation.makeSavedReqLocationAPIEntityList $ SavedReqLocation.makeSavedReqLocationAPIEntity <$> savedLocations

deleteSavedLocation :: Id Person.Person -> Text -> FlowHandler APISuccess.APISuccess
deleteSavedLocation riderId tag = withFlowHandlerAPI . withPersonIdLogTag riderId $ do
  runTransaction $
    QSavedReqLocation.deleteByRiderIdAndTag
      riderId
      tag
  return APISuccess.Success

buildSavedReqLocationAPIEntity :: MonadFlow m => SavedLocation.SavedReqLocationAPIEntity -> UTCTime -> Id Person.Person -> m SavedLocation.SavedReqLocation
buildSavedReqLocationAPIEntity SavedLocation.SavedReqLocationAPIEntity {..} now riderId = do
  let SearchLocation.SearchReqLocationAPIEntity {..} = address
  locId <- generateGUID
  return
    SavedLocation.SavedReqLocation
      { id = locId,
        createdAt = now,
        updatedAt = now,
        ..
      }
