{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Shared get/put logic for person default emergency contacts.
-- Apps (rider, driver) supply storage and conversion via callbacks so the
-- library stays free of app-specific Person/Merchant types.
module Safety.Domain.Action.UI.PersonDefaultEmergencyNumber
  ( getEmergencyContacts,
    putEmergencyContacts,
  )
where

import Kernel.Prelude
import qualified Safety.Domain.Types.Common as SafetyCommon
import qualified Safety.Domain.Types.SafetySettings as DSafety

-- | Get: load list, decrypt, map to API type.
-- Callbacks: getList personId, decrypt single domain item, domain -> api.
getEmergencyContacts ::
  (Monad m) =>
  (personId -> m [domain]) ->
  (domain -> m decrypted) ->
  (decrypted -> api) ->
  personId ->
  m [api]
getEmergencyContacts getList decrypt toApi personId = do
  domainList <- getList personId
  decList <- mapM decrypt domainList
  return $ toApi <$> decList

-- | Put: build domain list via callback, replaceAll, then sync
-- aggregatedRideShareSetting on safety_settings (first contact's option).
-- Callbacks: buildOne contactItem, replaceAll personId [domain], getSafety personId,
-- updateSafety settings, getShareTripOption domain.
putEmergencyContacts ::
  (Monad m) =>
  personId ->
  [contactItem] ->
  (contactItem -> m domain) ->
  (personId -> [domain] -> m ()) ->
  (personId -> m DSafety.SafetySettings) ->
  (DSafety.SafetySettings -> m ()) ->
  (domain -> Maybe SafetyCommon.RideShareOptions) ->
  m ()
putEmergencyContacts personId contactItems buildOne replaceAll getSafety updateSafety getShareTripOption = do
  domainList <- mapM buildOne contactItems
  replaceAll personId domainList
  safetySettings <- getSafety personId
  let aggregatedRideShareSetting = listToMaybe domainList >>= getShareTripOption
  let updated = safetySettings {DSafety.aggregatedRideShareSetting = aggregatedRideShareSetting}
  updateSafety updated
