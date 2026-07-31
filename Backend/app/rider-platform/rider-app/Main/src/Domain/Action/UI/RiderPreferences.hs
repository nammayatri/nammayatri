module Domain.Action.UI.RiderPreferences
  ( postRiderPreference,
    getRiderPreference,
    getAllRiderPreferences,
    deleteRiderPreference,
  )
where

import qualified API.Types.UI.RiderPreferences as API
import qualified Data.Geohash as Geohash
import qualified Data.Text as T
import qualified Domain.Types.Extra.RiderPreferences as RP
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.RiderPreferences as DRP
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RiderPreferences as QRP
import Tools.Error

postRiderPreference ::
  ( Kernel.Prelude.Maybe (Id Person.Person),
    Id Domain.Types.Merchant.Merchant
  ) ->
  API.RiderPreferenceReq ->
  Environment.Flow APISuccess.APISuccess
postRiderPreference (mbPersonId, _merchantId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  case req.preferenceType of
    RP.LOCATION_PICKUP -> do
      locData <- req.locationData & fromMaybeM (InvalidRequest "locationData is required for LOCATION_PICKUP")
      geohash <- Geohash.encode 8 (locData.sourceLat, locData.sourceLon) & fromMaybeM (InvalidRequest "Invalid source coordinates")
      let geohashText = T.pack geohash
      mbExisting <- QRP.findLocationPickupByGeohash personId geohashText
      now <- getCurrentTime
      case mbExisting of
        Just existing ->
          -- Same source location — overwrite the saved pickup point
          QRP.updateByPrimaryKey
            existing
              { DRP.preferenceData = RP.LocationPickupPreference (buildPickupData locData geohashText),
                DRP.updatedAt = now
              }
        Nothing -> do
          newId <- generateGUID
          person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
          QRP.create
            DRP.RiderPreferences
              { id = Id newId,
                riderId = personId,
                preferenceType = RP.LOCATION_PICKUP,
                preferenceData = RP.LocationPickupPreference (buildPickupData locData geohashText),
                merchantId = person.merchantId,
                merchantOperatingCityId = person.merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }
  pure APISuccess.Success
  where
    buildPickupData locData geohashText =
      RP.LocationPickupData
        { sourceGeohash = geohashText,
          sourceLat = locData.sourceLat,
          sourceLon = locData.sourceLon,
          sourceAddress = locData.sourceAddress,
          pickupLat = locData.pickupLat,
          pickupLon = locData.pickupLon,
          pickupAddress = locData.pickupAddress,
          pickupAddressSubtitle = locData.pickupAddressSubtitle
        }

getRiderPreference ::
  ( Kernel.Prelude.Maybe (Id Person.Person),
    Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Prelude.Maybe Kernel.Prelude.Double ->
  Kernel.Prelude.Maybe Kernel.Prelude.Double ->
  Environment.Flow API.RiderPreferencesResp
getRiderPreference (mbPersonId, _merchantId) mbSourceLat mbSourceLon = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  sourceLat <- mbSourceLat & fromMaybeM (InvalidRequest "sourceLat is required")
  sourceLon <- mbSourceLon & fromMaybeM (InvalidRequest "sourceLon is required")
  geohash <- Geohash.encode 8 (sourceLat, sourceLon) & fromMaybeM (InvalidRequest "Invalid source coordinates")
  let geohashText = T.pack geohash
  mbPref <- QRP.findLocationPickupByGeohash personId geohashText
  let locationPickups = maybe [] (pure . toLocationPickupRespData) mbPref
  pure API.RiderPreferencesResp {locationPickups}

getAllRiderPreferences ::
  ( Kernel.Prelude.Maybe (Id Person.Person),
    Id Domain.Types.Merchant.Merchant
  ) ->
  Environment.Flow API.AllRiderPreferencesResp
getAllRiderPreferences (mbPersonId, _merchantId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  allPrefs <- QRP.findAllByRiderId personId
  let locationPickups = map toLocationPickupRespData $ filter isLocationPickup allPrefs
  pure API.AllRiderPreferencesResp {locationPickups}
  where
    isLocationPickup pref = case pref.preferenceData of
      RP.LocationPickupPreference _ -> True

deleteRiderPreference ::
  ( Kernel.Prelude.Maybe (Id Person.Person),
    Id Domain.Types.Merchant.Merchant
  ) ->
  Id DRP.RiderPreferences ->
  Environment.Flow APISuccess.APISuccess
deleteRiderPreference (mbPersonId, _merchantId) preferenceId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  pref <- QRP.findByPrimaryKey preferenceId >>= fromMaybeM (InvalidRequest $ "RiderPreference not found: " <> preferenceId.getId)
  -- Ownership check: only the owner can delete their preference
  unless (pref.riderId == personId) $
    throwError $ InvalidRequest "Not authorised to delete this preference"
  QRP.deleteByRiderIdAndId personId preferenceId
  pure APISuccess.Success

toLocationPickupRespData :: DRP.RiderPreferences -> API.LocationPickupRespData
toLocationPickupRespData pref = case pref.preferenceData of
  RP.LocationPickupPreference d ->
    API.LocationPickupRespData
      { id = pref.id,
        sourceGeohash = d.sourceGeohash,
        sourceLat = d.sourceLat,
        sourceLon = d.sourceLon,
        sourceAddress = d.sourceAddress,
        pickupLat = d.pickupLat,
        pickupLon = d.pickupLon,
        pickupAddress = d.pickupAddress,
        pickupAddressSubtitle = d.pickupAddressSubtitle,
        createdAt = pref.createdAt,
        updatedAt = pref.updatedAt
      }
