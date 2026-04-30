module Safety.Domain.Action.UI.PersonDefaultEmergencyNumber
  ( ServiceHandle (..),
    HasEmergencyContactHandle (..),
    BuildEmergencyContactCtx (..),
    getEmergencyContacts,
    updateEmergencyContacts,
  )
where

import Data.List (nubBy)
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Safety.API.Types.UI.PersonDefaultEmergencyNumber as API
import qualified Safety.Domain.Types.Common as SafetyCommon
import qualified Safety.Domain.Types.PersonDefaultEmergencyNumber as SafetyPDEN
import Safety.Storage.BeamFlow
import qualified Safety.Storage.Queries.PersonDefaultEmergencyNumberExtra as QPersonDEN
import qualified Safety.Storage.Queries.SafetySettingsExtra as QSafetyExtra

-- | Platform-specific callbacks; each app provides one instance.
data ServiceHandle m = ServiceHandle
  { -- | Build domain entities from request items. Responsible for encryption,
    -- DB lookups (contact person), and platform-specific field defaults.
    buildContacts :: Id SafetyCommon.Person -> Id SafetyCommon.Merchant -> [API.EmergencyContactReq] -> m [SafetyPDEN.PersonDefaultEmergencyNumber],
    -- | Enrich a decrypted domain entity for the API response (e.g., onRide check).
    enrichContact :: SafetyPDEN.DecryptedPersonDefaultEmergencyNumber -> m API.EmergencyContact,
    -- | Validate the update request before any DB writes.
    -- Rider: validates max-contact-count; driver: no-op.
    validatePutRequest :: Id SafetyCommon.Person -> API.UpdateEmergencyContactsReq -> m (),
    -- | Fetch person-derived safety-settings defaults for aggregated-ride-share upsert.
    -- Rider: fetches Person row; driver: Nothing.
    getPersonDefaults :: Id SafetyCommon.Person -> m (Maybe QSafetyExtra.SafetySettingsPersonDefaults),
    -- | Side-effects after contacts are replaced in DB.
    -- Rider: sends "added as emergency contact" notification; driver: no-op.
    handleAfterPut :: Id SafetyCommon.Person -> [SafetyPDEN.PersonDefaultEmergencyNumber] -> [SafetyPDEN.PersonDefaultEmergencyNumber] -> m ()
  }

class HasEmergencyContactHandle r m | m -> r where
  getEmergencyContactHandle :: m (ServiceHandle m)

-- | Extract (personId, merchantId) from any auth tuple shape.
class BuildEmergencyContactCtx authToken m where
  extractContactIds :: authToken -> m (Id SafetyCommon.Person, Id SafetyCommon.Merchant)

-- Rider: (Id Person, Id Merchant)
instance Monad m => BuildEmergencyContactCtx (Id a, Id b) m where
  extractContactIds (p, m) = pure (cast p, cast m)

-- Driver: (Id Person, Id Merchant, Id MerchantOpCity)
instance Monad m => BuildEmergencyContactCtx (Id a, Id b, Id c) m where
  extractContactIds (p, m, _) = pure (cast p, cast m)

getEmergencyContacts ::
  (BeamFlow m r, EncFlow m r, MonadFlow m, HasEmergencyContactHandle r m, BuildEmergencyContactCtx authToken m) =>
  authToken ->
  m API.GetEmergencyContactsRes
getEmergencyContacts authToken = do
  (personId, _) <- extractContactIds authToken
  svcHandle <- getEmergencyContactHandle
  domainList <- runInReplica $ QPersonDEN.findAllByPersonId personId
  decList <- mapM decrypt domainList
  contacts <- mapM svcHandle.enrichContact decList
  pure API.GetEmergencyContactsRes {defaultEmergencyNumbers = contacts}

updateEmergencyContacts ::
  (BeamFlow m r, MonadFlow m, HasEmergencyContactHandle r m, BuildEmergencyContactCtx authToken m) =>
  authToken ->
  API.UpdateEmergencyContactsReq ->
  m APISuccess.APISuccess
updateEmergencyContacts authToken req = do
  (personId, merchantId) <- extractContactIds authToken
  svcHandle <- getEmergencyContactHandle
  svcHandle.validatePutRequest personId req
  oldList <- runInReplica $ QPersonDEN.findAllByPersonId personId
  let dedupedContacts = nubBy (\a b -> a.mobileNumber == b.mobileNumber) req.defaultEmergencyNumbers
  newList <- svcHandle.buildContacts personId merchantId dedupedContacts
  -- Sync aggregatedRideShareSetting: most-permissive option across all contacts.
  let aggregated = computeAggregatedRideShare newList
  defaults <- svcHandle.getPersonDefaults personId
  let updateInfo = QSafetyExtra.emptyUpdateEmergencyInfo {QSafetyExtra.aggregatedRideShare = aggregated}
  void $ QSafetyExtra.upsert personId updateInfo (QSafetyExtra.getDefaultSafetySettings personId defaults)
  QPersonDEN.replaceAll personId newList
  svcHandle.handleAfterPut personId newList oldList
  pure APISuccess.Success

-- | Most-permissive share option across all contacts.
-- ALWAYS_SHARE > SHARE_WITH_TIME_CONSTRAINTS > NEVER_SHARE / unset.
computeAggregatedRideShare :: [SafetyPDEN.PersonDefaultEmergencyNumber] -> Maybe SafetyCommon.RideShareOptions
computeAggregatedRideShare contacts =
  let opts = mapMaybe SafetyPDEN.shareTripWithEmergencyContactOption contacts
   in if SafetyCommon.ALWAYS_SHARE `elem` opts
        then Just SafetyCommon.ALWAYS_SHARE
        else
          if SafetyCommon.SHARE_WITH_TIME_CONSTRAINTS `elem` opts
            then Just SafetyCommon.SHARE_WITH_TIME_CONSTRAINTS
            else Nothing
