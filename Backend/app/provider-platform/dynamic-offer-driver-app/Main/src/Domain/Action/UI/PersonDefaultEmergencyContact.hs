{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.PersonDefaultEmergencyContact where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import Kernel.Prelude (UTCTime)
import qualified Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id (cast)
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EncFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Safety.API.Types.UI.PersonDefaultEmergencyNumber as API
import qualified Safety.Domain.Action.UI.PersonDefaultEmergencyNumber as EmergencyLib
import qualified Safety.Domain.Types.Common as SafetyCommon
import qualified Safety.Domain.Types.PersonDefaultEmergencyNumber as SafetyPDEN
import qualified Safety.Storage.Queries.PersonDefaultEmergencyNumber as LibQPDEN
import qualified Safety.Storage.Queries.SafetySettingsExtra as QSafetyExtra
import Servant
import Storage.Beam.Sos ()
import qualified Storage.Queries.PersonExtra as QPersonExtra
import Tools.Auth
import Tools.Error

instance EmergencyLib.HasEmergencyContactHandle AppEnv Flow where
  getEmergencyContactHandle =
    pure
      EmergencyLib.ServiceHandle
        { EmergencyLib.buildContacts = driverBuildContacts,
          EmergencyLib.enrichContact = driverEnrichContact,
          EmergencyLib.validatePutRequest = \_ _ -> pure (),
          EmergencyLib.getPersonDefaults = \_ -> pure Nothing,
          EmergencyLib.handleAfterPut = \_ _ _ -> pure ()
        }

driverBuildContacts ::
  Kernel.Types.Id.Id SafetyCommon.Person ->
  Kernel.Types.Id.Id SafetyCommon.Merchant ->
  [API.EmergencyContactReq] ->
  Flow [SafetyPDEN.PersonDefaultEmergencyNumber]
driverBuildContacts personId merchantId contacts = do
  now <- getCurrentTime
  mapM (buildOne now) contacts
  where
    buildOne ::
      UTCTime ->
      API.EmergencyContactReq ->
      Flow SafetyPDEN.PersonDefaultEmergencyNumber
    buildOne now contact = do
      encMobile <- encrypt contact.mobileNumber
      mobileNumberHash <- getDbHash contact.mobileNumber
      mbContactPerson <-
        QPersonExtra.findByMobileNumberAndMerchantAndRole
          contact.mobileCountryCode
          mobileNumberHash
          (cast merchantId)
          Domain.Types.Person.DRIVER
      pure
        SafetyPDEN.PersonDefaultEmergencyNumber
          { personId = cast personId,
            name = contact.name,
            mobileNumber = encMobile,
            mobileCountryCode = contact.mobileCountryCode,
            createdAt = now,
            contactPersonId = (cast . (.id)) <$> mbContactPerson,
            enableForFollowing = Kernel.Prelude.fromMaybe False contact.enableForFollowing,
            enableForShareRide = Kernel.Prelude.fromMaybe False contact.enableForShareRide,
            merchantId = Just (cast merchantId),
            priority = Kernel.Prelude.fromMaybe 0 contact.priority,
            shareTripWithEmergencyContactOption = contact.shareTripWithEmergencyContactOption
          }

driverEnrichContact ::
  SafetyPDEN.DecryptedPersonDefaultEmergencyNumber ->
  Flow API.EmergencyContact
driverEnrichContact pden =
  pure
    API.EmergencyContact
      { name = pden.name,
        mobileCountryCode = pden.mobileCountryCode,
        mobileNumber = pden.mobileNumber,
        priority = pden.priority,
        contactPersonId = cast <$> pden.contactPersonId,
        merchantId = cast <$> pden.merchantId,
        enableForFollowing = pden.enableForFollowing,
        enableForShareRide = pden.enableForShareRide,
        shareTripWithEmergencyContactOption = pden.shareTripWithEmergencyContactOption,
        onRide = Nothing
      }
