{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.PersonDefaultEmergencyNumber where

import API.Types.UI.PersonDefaultEmergencyNumber as API
import Data.List (nubBy)
import Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Person
import Domain.Types.PersonDefaultEmergencyNumber as DPDEN
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import Kernel.Prelude
import Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonDefaultEmergencyNumber as QPersonDEN

getProfileDefaultEmergencyNumbers ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id DM.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Flow API.DefaultEmergencyNumbersEntity
  )
getProfileDefaultEmergencyNumbers (mbPersonId, _, _) = do
  personId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  personENList <- B.runInReplica $ QPersonDEN.findAllByPersonId personId
  decPersonENList <- decrypt `mapM` personENList
  let contactList = map makePersonDefaultEmergencyNumberAPIEntity decPersonENList
  return $ DefaultEmergencyNumbersEntity contactList
  where
    makePersonDefaultEmergencyNumberAPIEntity :: DPDEN.DecryptedPersonDefaultEmergencyNumber -> API.PersonDefaultEmergencyNumber
    makePersonDefaultEmergencyNumberAPIEntity DPDEN.PersonDefaultEmergencyNumber {..} =
      API.PersonDefaultEmergencyNumber
        { ..
        }

postProfileDefaultEmergencyNumbers ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id DM.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.DefaultEmergencyNumbersEntity ->
    Flow APISuccess.APISuccess
  )
postProfileDefaultEmergencyNumbers (mbPersonId, merchantId, merchantOperatingCityId) API.DefaultEmergencyNumbersEntity {..} = do
  now <- getCurrentTime
  personId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  let uniqueRecords = nubBy ((==) `on` (.mobileNumber)) defaultEmergencyNumbers
  personENList <- buildPersonDefaultEmergencyNumber now personId `mapM` uniqueRecords
  B.runInReplica $ replaceAll personId personENList
  fork "Update Safety Setup" $ QP.updateSafetySetup personId True
  return APISuccess.Success
  where
    buildPersonDefaultEmergencyNumber now personId defEmNum = do
      encMobNum <- encrypt defEmNum.mobileNumber
      return $
        DPDEN.PersonDefaultEmergencyNumber
          { mobileNumber = encMobNum,
            name = defEmNum.name,
            mobileCountryCode = defEmNum.mobileCountryCode,
            createdAt = now,
            updatedAt = now,
            merchantId = merchantId,
            merchantOperatingCityId = Just merchantOperatingCityId,
            ..
          }

replaceAll :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id Person -> [DPDEN.PersonDefaultEmergencyNumber] -> m ()
replaceAll (Id personId) contacts = do
  QPersonDEN.deleteAllByPersonId $ Id personId
  QPersonDEN.createMany contacts
