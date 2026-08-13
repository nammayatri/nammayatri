module Domain.Action.UI.RiderPreferences
  ( postRiderPreference,
    getRiderPreference,
    getRiderPreferenceRideConfig,
    getRiderPreferenceAll,
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
    RP.RIDE_CONFIG -> do
      cfg <- req.rideConfigData & fromMaybeM (InvalidRequest "rideConfigData is required for RIDE_CONFIG")
      now <- getCurrentTime
      mbExisting <- Kernel.Prelude.listToMaybe <$> QRP.findByRiderIdAndType personId RP.RIDE_CONFIG
      case mbExisting of
        Just existing -> do
          let existingCfg = case existing.preferenceData of
                RP.RideConfigPreference d -> Kernel.Prelude.Just d
                RP.LocationPickupPreference _ -> Kernel.Prelude.Nothing
          QRP.updateRideConfigByRiderId personId (mergeRideConfigData existingCfg cfg)
        Nothing -> do
          newId <- generateGUID
          person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
          QRP.create
            DRP.RiderPreferences
              { id = Id newId,
                riderId = personId,
                preferenceType = RP.RIDE_CONFIG,
                preferenceData = RP.RideConfigPreference (buildRideConfigData cfg),
                merchantId = person.merchantId,
                merchantOperatingCityId = person.merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }
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
    buildRideConfigData cfg =
      RP.RideConfigData
        { isPetRide = cfg.isPetRide,
          isBusiness = cfg.isBusiness,
          isTransitEnabled = cfg.isTransitEnabled,
          isAutoAssign = cfg.isAutoAssign
        }
    mergeRideConfigData mbOld new =
      RP.RideConfigData
        { isPetRide = new.isPetRide <|> (mbOld >>= (.isPetRide)),
          isBusiness = new.isBusiness <|> (mbOld >>= (.isBusiness)),
          isTransitEnabled = new.isTransitEnabled <|> (mbOld >>= (.isTransitEnabled)),
          isAutoAssign = new.isAutoAssign <|> (mbOld >>= (.isAutoAssign))
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
  let locationPickups = maybeToList (mbPref >>= toLocationPickupRespData)
  pure API.RiderPreferencesResp {locationPickups}

getRiderPreferenceRideConfig ::
  ( Kernel.Prelude.Maybe (Id Person.Person),
    Id Domain.Types.Merchant.Merchant
  ) ->
  Environment.Flow API.RideConfigData
getRiderPreferenceRideConfig (mbPersonId, _merchantId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  mbPref <- Kernel.Prelude.listToMaybe <$> QRP.findByRiderIdAndType personId RP.RIDE_CONFIG
  pure $ Kernel.Prelude.fromMaybe emptyRideConfig (mbPref >>= toRideConfigData)
  where
    emptyRideConfig =
      API.RideConfigData
        { isPetRide = Kernel.Prelude.Nothing,
          isBusiness = Kernel.Prelude.Nothing,
          isTransitEnabled = Kernel.Prelude.Nothing,
          isAutoAssign = Kernel.Prelude.Nothing
        }

getRiderPreferenceAll ::
  ( Kernel.Prelude.Maybe (Id Person.Person),
    Id Domain.Types.Merchant.Merchant
  ) ->
  Environment.Flow API.AllRiderPreferencesResp
getRiderPreferenceAll (mbPersonId, _merchantId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  allPrefs <- QRP.findAllByRiderId personId
  let locationPickups = mapMaybe toLocationPickupRespData allPrefs
      rideConfig = Kernel.Prelude.listToMaybe (mapMaybe toRideConfigData allPrefs)
  pure API.AllRiderPreferencesResp {locationPickups, rideConfig}

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

toLocationPickupRespData :: DRP.RiderPreferences -> Kernel.Prelude.Maybe API.LocationPickupRespData
toLocationPickupRespData pref = case pref.preferenceData of
  RP.LocationPickupPreference d ->
    Kernel.Prelude.Just
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
  RP.RideConfigPreference _ -> Kernel.Prelude.Nothing

toRideConfigData :: DRP.RiderPreferences -> Kernel.Prelude.Maybe API.RideConfigData
toRideConfigData pref = case pref.preferenceData of
  RP.RideConfigPreference d ->
    Kernel.Prelude.Just
      API.RideConfigData
        { isPetRide = d.isPetRide,
          isBusiness = d.isBusiness,
          isTransitEnabled = d.isTransitEnabled,
          isAutoAssign = d.isAutoAssign
        }
  RP.LocationPickupPreference _ -> Kernel.Prelude.Nothing
