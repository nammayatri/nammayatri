{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.Dashboard.AppManagement.Customer (postCustomerSosCreate, postCustomerDeletedPerson, getCustomerSavedLocations, postCustomerSavedLocations, deleteCustomerSavedLocations) where

import qualified API.Types.Dashboard.AppManagement.Endpoints.Customer as Customer
import qualified API.Types.UI.DeletedPerson
import qualified "this" API.Types.UI.Sos
import qualified Domain.Action.UI.DeletedPerson
import qualified Domain.Action.UI.SavedReqLocation as DSavedReqLocation
import qualified Domain.Action.UI.Sos
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import Domain.Types.SavedReqLocation (SavedReqLocationAPIEntity (..))
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions (runInReplica)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.SavedReqLocation as QSavedReqLocation

postCustomerSosCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.Sos.SosReq -> Environment.Flow API.Types.UI.Sos.SosRes)
postCustomerSosCreate merchantShortId _opCity personId req = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.Sos.postSosCreate (Just personId, m.id) req

postCustomerDeletedPerson :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.UI.DeletedPerson.DeletedPersonReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postCustomerDeletedPerson merchantShortId _opCity personId req = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.DeletedPerson.postDeletedPerson (Just personId, m.id) req

getCustomerSavedLocations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.Flow Customer.SavedReqLocationsListRes)
getCustomerSavedLocations merchantShortId _opCity personId = do
  _m <- findMerchantByShortId merchantShortId
  savedLocations <- runInReplica $ QSavedReqLocation.findAllByRiderId personId
  return $ Customer.SavedReqLocationsListRes $ makeSavedReqLocationAPIEntity <$> savedLocations
  where
    makeSavedReqLocationAPIEntity loc =
      SavedReqLocationAPIEntity
        { area = loc.area,
          areaCode = loc.areaCode,
          building = loc.building,
          city = loc.city,
          country = loc.country,
          door = loc.door,
          lat = loc.lat,
          locationName = loc.locationName,
          lon = loc.lon,
          placeId = loc.placeId,
          state = loc.state,
          street = loc.street,
          tag = loc.tag,
          ward = loc.ward
        }

postCustomerSavedLocations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Customer.CreateSavedReqLocationReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postCustomerSavedLocations merchantShortId _opCity personId req = do
  _m <- findMerchantByShortId merchantShortId
  let createReq =
        DSavedReqLocation.CreateSavedReqLocationReq
          { lat = req.lat,
            lon = req.lon,
            street = req.street,
            door = req.door,
            city = req.city,
            state = req.state,
            country = req.country,
            building = req.building,
            areaCode = req.areaCode,
            area = req.area,
            tag = req.tag,
            placeId = req.placeId,
            ward = req.ward,
            isMoved = req.isMoved,
            locationName = req.locationName
          }
  DSavedReqLocation.createSavedReqLocation personId createReq

deleteCustomerSavedLocations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteCustomerSavedLocations merchantShortId _opCity personId tag = do
  _m <- findMerchantByShortId merchantShortId
  DSavedReqLocation.deleteSavedReqLocation personId tag

instance Kernel.Types.HideSecrets.HideSecrets API.Types.UI.Sos.SosReq where
  hideSecrets = Kernel.Prelude.identity

instance Kernel.Types.HideSecrets.HideSecrets API.Types.UI.DeletedPerson.DeletedPersonReq where
  hideSecrets = Kernel.Prelude.identity

instance Kernel.Types.HideSecrets.HideSecrets Customer.CreateSavedReqLocationReq where
  hideSecrets = Kernel.Prelude.identity

