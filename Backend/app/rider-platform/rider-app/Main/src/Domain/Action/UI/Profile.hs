{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Profile
  ( ProfileRes,
    UpdateProfileReq (..),
    UpdateProfileResp,
    UpdateProfileDefaultEmergencyNumbersReq (..),
    PersonDefaultEmergencyNumber (..),
    UpdateProfileDefaultEmergencyNumbersResp,
    GetProfileDefaultEmergencyNumbersResp (..),
    getPersonDetails,
    updatePerson,
    updateDefaultEmergencyNumbers,
    getDefaultEmergencyNumbers,
  )
where

import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.PersonDefaultEmergencyNumber as DPDEN
import Environment
import Kernel.External.Encryption
import qualified Kernel.External.FCM.Types as FCM
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runInReplica, runTransaction)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.Validation
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import qualified Kernel.Utils.Text as TU
import Kernel.Utils.Validation
import SharedLogic.CallBPPInternal as CallBPPInternal
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Person.PersonDefaultEmergencyNumber as QPersonDEN
import Storage.Tabular.Person ()
import Tools.Error
import Tools.Metrics

type ProfileRes = Person.PersonAPIEntity

data UpdateProfileReq = UpdateProfileReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    deviceToken :: Maybe FCM.FCMRecipientToken,
    referralCode :: Maybe Text,
    language :: Maybe Maps.Language
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type UpdateProfileResp = APISuccess.APISuccess

newtype UpdateProfileDefaultEmergencyNumbersReq = UpdateProfileDefaultEmergencyNumbersReq
  { defaultEmergencyNumbers :: [PersonDefaultEmergencyNumber]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

validateUpdateProfileDefaultEmergencyNumbersReq :: Int -> Validate UpdateProfileDefaultEmergencyNumbersReq
validateUpdateProfileDefaultEmergencyNumbersReq maxEmergencyNumberCount UpdateProfileDefaultEmergencyNumbersReq {..} =
  sequenceA_
    [ validateList "defaultEmergencyNumbers" defaultEmergencyNumbers validatePersonDefaultEmergencyNumber,
      validateField "defaultEmergencyNumbers" defaultEmergencyNumbers $ MaxLength maxEmergencyNumberCount
    ]

data PersonDefaultEmergencyNumber = PersonDefaultEmergencyNumber
  { name :: Text,
    mobileCountryCode :: Text,
    mobileNumber :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

validatePersonDefaultEmergencyNumber :: Validate PersonDefaultEmergencyNumber
validatePersonDefaultEmergencyNumber PersonDefaultEmergencyNumber {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileIndianCode
    ]

type UpdateProfileDefaultEmergencyNumbersResp = APISuccess.APISuccess

newtype GetProfileDefaultEmergencyNumbersResp = GetProfileDefaultEmergencyNumbersResp
  { defaultEmergencyNumbers :: [DPDEN.PersonDefaultEmergencyNumberAPIEntity]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getPersonDetails :: (EsqDBReplicaFlow m r, EncFlow m r) => Id Person.Person -> m ProfileRes
getPersonDetails personId = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  decPerson <- decrypt person
  return $ Person.makePersonAPIEntity decPerson

updatePerson :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasBapInfo r m, CoreMetrics m) => Id Person.Person -> UpdateProfileReq -> m APISuccess.APISuccess
updatePerson personId req = do
  mPerson <- join <$> QPerson.findByEmail `mapM` req.email
  whenJust mPerson (\_ -> throwError PersonEmailExists)
  mbEncEmail <- encrypt `mapM` req.email

  refCode <- join <$> validateRefferalCode personId `mapM` req.referralCode
  runTransaction $
    QPerson.updatePersonalInfo
      personId
      (req.firstName)
      (req.middleName)
      (req.lastName)
      refCode
      mbEncEmail
      (req.deviceToken)
      (req.language)
  pure APISuccess.Success

validateRefferalCode :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasBapInfo r m, CoreMetrics m) => Id Person.Person -> Text -> m (Maybe Text)
validateRefferalCode personId refCode = do
  unless (TU.validateAllDigitWithMinLength 6 refCode) (throwError $ InvalidRequest "Referral Code must have 6 digits")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId) >>= decrypt
  when (person.hasTakenValidRide) do
    throwError (InvalidRequest "You have already reffered someone")
  case person.referralCode of
    Just code ->
      if code /= refCode
        then throwError (InvalidRequest "Referral Code is not same")
        else return Nothing -- idempotent behaviour
    Nothing -> do
      merchant <- QMerchant.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
      case (person.mobileNumber, person.mobileCountryCode) of
        (Just mobileNumber, Just countryCode) -> do
          void $ CallBPPInternal.linkReferee merchant.driverOfferApiKey merchant.driverOfferBaseUrl merchant.driverOfferMerchantId refCode mobileNumber countryCode
          return $ Just refCode
        _ -> throwError (InvalidRequest "Mobile number is null")

updateDefaultEmergencyNumbers ::
  ( EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["maxEmergencyNumberCount" ::: Int]
  ) =>
  Id Person.Person ->
  UpdateProfileDefaultEmergencyNumbersReq ->
  m UpdateProfileDefaultEmergencyNumbersResp
updateDefaultEmergencyNumbers personId req = do
  maxEmergencyNumberCount <- asks (.maxEmergencyNumberCount)
  runRequestValidation (validateUpdateProfileDefaultEmergencyNumbersReq maxEmergencyNumberCount) req
  now <- getCurrentTime
  newPersonDENList <- buildPersonDefaultEmergencyNumber now `mapM` req.defaultEmergencyNumbers
  runTransaction $ QPersonDEN.replaceAll personId newPersonDENList
  pure APISuccess.Success
  where
    buildPersonDefaultEmergencyNumber now defEmNum = do
      encMobNum <- encrypt defEmNum.mobileNumber
      return $
        DPDEN.PersonDefaultEmergencyNumber
          { mobileNumber = encMobNum,
            name = defEmNum.name,
            mobileCountryCode = defEmNum.mobileCountryCode,
            createdAt = now,
            ..
          }

getDefaultEmergencyNumbers :: (EsqDBReplicaFlow m r, EncFlow m r) => Id Person.Person -> m GetProfileDefaultEmergencyNumbersResp
getDefaultEmergencyNumbers personId = do
  personENList <- runInReplica $ QPersonDEN.findAllByPersonId personId
  decPersonENList <- decrypt `mapM` personENList
  return . GetProfileDefaultEmergencyNumbersResp $ DPDEN.makePersonDefaultEmergencyNumberAPIEntity <$> decPersonENList
