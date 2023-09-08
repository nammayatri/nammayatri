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

import Control.Applicative ((<|>))
import Data.List (nubBy)
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.PersonDefaultEmergencyNumber as DPDEN
import qualified Domain.Types.Person.PersonDisability as PersonDisability
import Kernel.Beam.Functions
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.Validation
import Kernel.Types.Version
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import qualified Kernel.Utils.Text as TU
import Kernel.Utils.Validation
import SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Storage.CachedQueries.Merchant.MerchantConfigNew as QMCN
import qualified Storage.Queries.Disability as QD
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Person.PersonDefaultEmergencyNumber as QPersonDEN
import qualified Storage.Queries.Person.PersonDisability as PDisability
import Tools.Error

type ProfileRes = Person.PersonAPIEntity

data UpdateProfileReq = UpdateProfileReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    deviceToken :: Maybe Text,
    notificationToken :: Maybe Text,
    referralCode :: Maybe Text,
    language :: Maybe Maps.Language,
    gender :: Maybe Person.Gender,
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version,
    disability :: Maybe Disability,
    hasDisability :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type UpdateProfileResp = APISuccess.APISuccess

data Disability = Disability
  { id :: Id Disability,
    description :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToSchema, ToJSON, FromJSON)

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
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

type UpdateProfileDefaultEmergencyNumbersResp = APISuccess.APISuccess

newtype GetProfileDefaultEmergencyNumbersResp = GetProfileDefaultEmergencyNumbersResp
  { defaultEmergencyNumbers :: [DPDEN.PersonDefaultEmergencyNumberAPIEntity]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getPersonDetails :: (EsqDBReplicaFlow m r, EncFlow m r) => (Id Person.Person, Id Merchant.Merchant) -> m ProfileRes
getPersonDetails (personId, _) = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  customerDisability <- B.runInReplica $ PDisability.findByPersonId personId
  let tag = customerDisability <&> (.tag)
  decPerson <- decrypt person
  return $ Person.makePersonAPIEntity decPerson tag

updatePerson :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Id Person.Person -> UpdateProfileReq -> m APISuccess.APISuccess
updatePerson personId req = do
  mPerson <- join <$> QPerson.findByEmail `mapM` req.email
  whenJust mPerson (\_ -> throwError PersonEmailExists)
  mbEncEmail <- encrypt `mapM` req.email

  refCode <- join <$> validateRefferalCode personId `mapM` req.referralCode

  void $
    QPerson.updatePersonalInfo
      personId
      req.firstName
      req.middleName
      req.lastName
      refCode
      mbEncEmail
      req.deviceToken
      req.notificationToken
      req.language
      req.gender
      req.clientVersion
      req.bundleVersion
  updateDisability req.hasDisability req.disability personId

updateDisability :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Maybe Bool -> Maybe Disability -> Id Person.Person -> m APISuccess.APISuccess
updateDisability hasDisability mbDisability personId = do
  case (hasDisability, mbDisability) of
    (Nothing, _) -> logDebug "No Disability"
    (Just False, _) -> QPerson.updateHasDisability personId $ Just False
    (Just True, Nothing) -> throwError $ InvalidRequest "Field disability can't be null if hasDisability is True"
    (Just True, Just selectedDisability) -> do
      customerDisability <- B.runInReplica $ PDisability.findByPersonId personId
      QPerson.updateHasDisability personId $ Just True
      let disabilityId = getId $ selectedDisability.id
      disability <- runInReplica $ QD.findByDisabilityId disabilityId >>= fromMaybeM (DisabilityDoesNotExist disabilityId)
      let mbDescription = (selectedDisability.description) <|> (Just disability.description)
      when (isNothing customerDisability) $ do
        newDisability <- makeDisability selectedDisability disability.tag mbDescription
        PDisability.create newDisability
      when (isJust customerDisability) $ do
        PDisability.updateDisabilityByPersonId personId disabilityId disability.tag mbDescription
      where
        makeDisability personDisability tag mbDescription = do
          now <- getCurrentTime
          return $
            PersonDisability.PersonDisability
              { personId = personId,
                disabilityId = getId $ personDisability.id,
                tag = tag,
                description = mbDescription,
                updatedAt = now
              }
  pure APISuccess.Success

validateRefferalCode :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Id Person.Person -> Text -> m (Maybe Text)
validateRefferalCode personId refCode = do
  unless (TU.validateAllDigitWithMinLength 6 refCode) (throwError $ InvalidRequest "Referral Code must have 6 digits")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId) >>= decrypt
  when (person.hasTakenValidRide) do
    throwError (InvalidRequest "You have been already referred by someone")
  case person.referralCode of
    Just code ->
      if code /= refCode
        then throwError (InvalidRequest "Referral Code is not same")
        else return Nothing -- idempotent behaviour
    Nothing -> do
      merchantConfig <- QMCN.findByMerchantId person.merchantId >>= fromMaybeM (MerchantDoesNotExist person.merchantId.getId)
      case (person.mobileNumber, person.mobileCountryCode) of
        (Just mobileNumber, Just countryCode) -> do
          void $ CallBPPInternal.linkReferee merchantConfig.driverOfferApiKey merchantConfig.driverOfferBaseUrl merchantConfig.driverOfferMerchantId refCode mobileNumber countryCode
          return $ Just refCode
        _ -> throwError (InvalidRequest "Mobile number is null")

updateDefaultEmergencyNumbers ::
  ( EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["maxEmergencyNumberCount" ::: Int]
  ) =>
  Id Person.Person ->
  UpdateProfileDefaultEmergencyNumbersReq ->
  m UpdateProfileDefaultEmergencyNumbersResp
updateDefaultEmergencyNumbers personId req = do
  maxEmergencyNumberCount <- asks (.maxEmergencyNumberCount)
  runRequestValidation (validateUpdateProfileDefaultEmergencyNumbersReq maxEmergencyNumberCount) req
  now <- getCurrentTime
  let uniqueRecords = getUniquePersonByMobileNumber req
  newPersonDENList <- buildPersonDefaultEmergencyNumber now `mapM` uniqueRecords
  QPersonDEN.replaceAll personId newPersonDENList
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

getDefaultEmergencyNumbers :: (EsqDBReplicaFlow m r, EncFlow m r) => (Id Person.Person, Id Merchant.Merchant) -> m GetProfileDefaultEmergencyNumbersResp
getDefaultEmergencyNumbers (personId, _) = do
  personENList <- runInReplica $ QPersonDEN.findAllByPersonId personId
  decPersonENList <- decrypt `mapM` personENList
  return . GetProfileDefaultEmergencyNumbersResp $ DPDEN.makePersonDefaultEmergencyNumberAPIEntity <$> decPersonENList

getUniquePersonByMobileNumber :: UpdateProfileDefaultEmergencyNumbersReq -> [PersonDefaultEmergencyNumber]
getUniquePersonByMobileNumber req =
  nubBy ((==) `on` mobileNumber) req.defaultEmergencyNumbers
