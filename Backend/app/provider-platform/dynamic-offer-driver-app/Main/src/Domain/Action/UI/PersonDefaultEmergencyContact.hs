{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.PersonDefaultEmergencyContact
  ( getDriverPersonDefaultEmergencyContacts,
    putDriverPersonDefaultEmergencyContacts,
  )
where

import qualified API.Types.UI.PersonDefaultEmergencyContact as API
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import Kernel.Prelude (UTCTime)
import qualified Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id (cast)
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EncFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Safety.Domain.Action.UI.PersonDefaultEmergencyNumber as LibPDEN
import qualified Safety.Domain.Types.PersonDefaultEmergencyNumber as SafetyPDEN
import qualified Safety.Storage.Queries.PersonDefaultEmergencyNumber as LibQPDEN
import qualified Safety.Storage.Queries.SafetySettings as QSafetySettings
import qualified Safety.Storage.Queries.SafetySettingsExtra as Lib
import Servant
import Storage.Beam.Sos ()
import qualified Storage.Queries.PersonExtra as QPersonExtra
import Tools.Auth
import Tools.Error

getDriverPersonDefaultEmergencyContacts ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow [API.PersonDefaultEmergencyContact]
  )
getDriverPersonDefaultEmergencyContacts (mbPersonId, _, _) = do
  personId <- mbPersonId & fromMaybeM (PersonDoesNotExist "No person found")
  LibPDEN.getEmergencyContacts
    (runInReplica . LibQPDEN.findAllByPersonId . cast)
    decrypt
    toAPIContact
    personId
  where
    toAPIContact :: SafetyPDEN.DecryptedPersonDefaultEmergencyNumber -> API.PersonDefaultEmergencyContact
    toAPIContact pden =
      API.PersonDefaultEmergencyContact
        { name = pden.name,
          mobileCountryCode = pden.mobileCountryCode,
          mobileNumber = pden.mobileNumber,
          priority = pden.priority,
          contactPersonId = cast <$> pden.contactPersonId,
          merchantId = cast <$> pden.merchantId,
          enableForFollowing = pden.enableForFollowing,
          enableForShareRide = pden.enableForShareRide,
          shareTripWithEmergencyContactOption = pden.shareTripWithEmergencyContactOption
        }

putDriverPersonDefaultEmergencyContacts ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.UpdatePersonDefaultEmergencyContactsReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
putDriverPersonDefaultEmergencyContacts (mbPersonId, merchantId, _) req = do
  personId <- mbPersonId & fromMaybeM (PersonDoesNotExist "No person found")
  now <- getCurrentTime
  LibPDEN.putEmergencyContacts
    personId
    req.emergencyContacts
    (buildPersonDefaultEmergencyNumber now personId merchantId)
    (LibQPDEN.replaceAll . cast)
    (\pid -> Lib.findSafetySettingsWithFallback (cast pid) (Lib.getDefaultSafetySettings (cast pid) Nothing))
    QSafetySettings.updateByPrimaryKey
    SafetyPDEN.shareTripWithEmergencyContactOption
  pure Kernel.Types.APISuccess.Success
  where
    buildPersonDefaultEmergencyNumber ::
      (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
      UTCTime ->
      Kernel.Types.Id.Id Domain.Types.Person.Person ->
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
      API.PersonDefaultEmergencyContact ->
      m SafetyPDEN.PersonDefaultEmergencyNumber
    buildPersonDefaultEmergencyNumber now personId mId apiContact = do
      encMobile <- encrypt apiContact.mobileNumber
      mobileNumberHash <- getDbHash apiContact.mobileNumber
      mbContactPerson <- QPersonExtra.findByMobileNumberAndMerchantAndRole apiContact.mobileCountryCode mobileNumberHash mId Domain.Types.Person.DRIVER
      return $
        SafetyPDEN.PersonDefaultEmergencyNumber
          { personId = cast personId,
            name = apiContact.name,
            mobileNumber = encMobile,
            mobileCountryCode = apiContact.mobileCountryCode,
            createdAt = now,
            contactPersonId = (cast . (.id)) <$> mbContactPerson,
            enableForFollowing = apiContact.enableForFollowing,
            enableForShareRide = apiContact.enableForShareRide,
            merchantId = Just (cast mId),
            priority = apiContact.priority,
            shareTripWithEmergencyContactOption = apiContact.shareTripWithEmergencyContactOption
          }
