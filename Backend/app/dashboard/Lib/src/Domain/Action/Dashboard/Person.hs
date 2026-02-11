{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Person where

import Dashboard.Common
import Data.Char (isDigit, isLower, isUpper)
import Data.List (groupBy, nub, sortOn)
import qualified Data.Text as T
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantAccess as DAccess
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.API as AP
import qualified Domain.Types.Person.Type as DPT
import qualified Domain.Types.Person.Type as SP
import qualified Domain.Types.Role as DRole
import qualified Domain.Types.ServerName as DTServer
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import Storage.Beam.BeamFlow
import qualified Storage.Queries.AccessMatrix as QMatrix
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.Person as QP
import qualified "dynamic-offer-driver-app" Storage.Queries.Person as QPerson
import qualified Storage.Queries.RegistrationToken as QReg
import qualified Storage.CachedQueries.Role as CQRole
import Tools.Auth
import qualified Tools.Auth.Common as Auth
import Tools.Auth.Merchant
import Tools.Error

data ListPersonRes = ListPersonRes
  { list :: [DP.PersonAPIEntity],
    summary :: Summary
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype MerchantAccessReq = MerchantAccessReq
  { merchantId :: ShortId DMerchant.Merchant
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MerchantCityAccessReq = MerchantCityAccessReq
  { merchantId :: ShortId DMerchant.Merchant,
    operatingCity :: City.City
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type MerchantAccessRes = MerchantCityAccessReq

data ChangePasswordReq = ChangePasswordReq
  { oldPassword :: Text,
    newPassword :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ChangePasswordAfterExpiryReq = ChangePasswordAfterExpiryReq
  { email :: Text,
    oldPassword :: Text,
    newPassword :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CreatePersonReq = CreatePersonReq
  { firstName :: Text,
    lastName :: Text,
    roleId :: Id DRole.Role,
    email :: Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    password :: Text,
    dashboardType :: Maybe DPT.DashboardType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype ChangeEmailByAdminReq = ChangeEmailByAdminReq
  { newEmail :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype ChangeMobileNumberByAdminReq = ChangeMobileNumberByAdminReq
  { newMobileNumber :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype ChangePasswordByAdminReq = ChangePasswordByAdminReq
  { newPassword :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype ReleaseRegisterReq = ReleaseRegisterReq
  {token :: Text}
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype ChangeEnabledStatusReq = ChangeEnabledStatusReq
  { enabled :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ReleaseRegisterRes = ReleaseRegisterRes
  { username :: Text,
    token :: Text,
    otpEnabled :: Bool,
    merchantId :: Maybe Text,
    email :: Text,
    context :: Text,
    acl :: Maybe Text,
    merchantTrack :: Maybe Text,
    clientConfig :: Maybe Text,
    resellerId :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data GetProductSpecInfoResp = GetProductSpecInfoResp
  { merchant_id :: Text,
    client_id :: Text,
    platform :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

registerRelease ::
  ( BeamFlow m r,
    EncFlow m r
  ) =>
  TokenInfo ->
  ReleaseRegisterReq ->
  m ReleaseRegisterRes
registerRelease _ ReleaseRegisterReq {..} = do
  return
    ReleaseRegisterRes
      { username = "Sidharth",
        token = token,
        otpEnabled = False,
        merchantId = Just "merchantId",
        email = "sidharth.sethu@juspay.in",
        context = "JUSPAY",
        acl = Just "{\"mjos_manager\":\"RW\"}",
        merchantTrack = Nothing,
        clientConfig = Nothing,
        resellerId = Nothing
      }

getProductSpecInfo ::
  BeamFlow m r =>
  Maybe Text ->
  m GetProductSpecInfoResp
getProductSpecInfo _ = do
  return
    GetProductSpecInfoResp
      { merchant_id = "nammayatriconsumer",
        client_id = "nammayatriconsumer",
        platform = "android"
      }

validateCreatePerson :: Validate CreatePersonReq
validateCreatePerson CreatePersonReq {..} =
  sequenceA_
    [ validateField "firstName" firstName $ MinLength 3 `And` P.name,
      validateField "lastName" lastName $ NotEmpty `And` P.name,
      validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

validateChangeMobileNumberReq :: Validate ChangeMobileNumberByAdminReq
validateChangeMobileNumberReq ChangeMobileNumberByAdminReq {..} =
  sequenceA_
    [ validateField "mobileNumber" newMobileNumber P.mobileNumber
    ]

newtype CreatePersonRes = CreatePersonRes
  {person :: AP.PersonAPIEntity}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

createPerson ::
  (BeamFlow m r, EncFlow m r) =>
  TokenInfo ->
  CreatePersonReq ->
  m CreatePersonRes
createPerson _ personEntity = do
  runRequestValidation validateCreatePerson personEntity
  unlessM
    ( isNothing
        <$> DPT.withDashboardType personEntity.dashboardType
          (\(_ :: Proxy t) -> QP.findByEmailWithType @t personEntity.email)
    )
    $ throwError (InvalidRequest "Email already registered")
  unlessM
    ( isNothing
        <$> DPT.withDashboardType personEntity.dashboardType
          (\(_ :: Proxy t) -> QP.findByMobileNumberWithType @t personEntity.mobileNumber personEntity.mobileCountryCode)
    )
    $ throwError (InvalidRequest "Phone already registered")
  let roleId = personEntity.roleId
  role <- CQRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)
  person <- buildPerson personEntity (role.dashboardAccessType)
  decPerson <- decrypt person
  let personAPIEntity = AP.makePersonAPIEntity decPerson role [] Nothing
  QP.create person
  return $ CreatePersonRes personAPIEntity

listPerson ::
  (BeamFlow m r, EncFlow m r) =>
  TokenInfo ->
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe (Id DP.Person) ->
  m ListPersonRes
listPerson _ mbSearchString mbLimit mbOffset mbPersonId = do
  mbSearchStrDBHash <- getDbHash `traverse` mbSearchString
  personAndRoleList <- B.runInReplica $ QP.findAllWithLimitOffset mbSearchString mbSearchStrDBHash mbLimit mbOffset mbPersonId
  res <- forM personAndRoleList $ \(encPerson, role, merchantAccessList, merchantCityAccessList) -> do
    decPerson <- decrypt encPerson
    let availableCitiesForMerchant = makeAvailableCitiesForMerchant merchantAccessList merchantCityAccessList
    pure $ DP.makePersonAPIEntity decPerson role (nub merchantAccessList) (Just availableCitiesForMerchant)
  let count = length res
  let summary = Summary {totalCount = 10000, count}
  pure $ ListPersonRes {list = res, summary = summary}

makeAvailableCitiesForMerchant :: [ShortId DMerchant.Merchant] -> [City.City] -> [DP.AvailableCitiesForMerchant]
makeAvailableCitiesForMerchant merchantAccessList merchantCityAccessList = do
  let merchantCityList = sortOn fst $ zip merchantAccessList merchantCityAccessList
  let groupedByMerchant = groupBy ((==) `on` fst) merchantCityList
  if null groupedByMerchant
    then []
    else do
      let merchantAccesslistWithCity = map (\group -> DP.AvailableCitiesForMerchant (fst (head group)) (map snd group)) groupedByMerchant
      merchantAccesslistWithCity

assignRole ::
  BeamFlow m r =>
  TokenInfo ->
  Id DP.Person ->
  Id DRole.Role ->
  m APISuccess
assignRole _ personId roleId = do
  _person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  role <- CQRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)
  QP.updatePersonRole personId role
  pure Success

assignMerchantCityAccess ::
  ( BeamFlow m r,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]]
  ) =>
  TokenInfo ->
  Id DP.Person ->
  MerchantCityAccessReq ->
  m APISuccess
assignMerchantCityAccess _ personId req = do
  merchant <-
    QMerchant.findByShortId req.merchantId
      >>= fromMaybeM (MerchantDoesNotExist req.merchantId.getShortId)
  merchantServerAccessCheck merchant
  let isSupportedCity = req.operatingCity `elem` (merchant.supportedOperatingCities)
  unless isSupportedCity $
    throwError $ InvalidRequest "Server does not support this city"
  _person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  mbMerchantAccess <- QAccess.findByPersonIdAndMerchantIdAndCity personId merchant.id req.operatingCity
  whenJust mbMerchantAccess $ \_ -> do
    throwError $ InvalidRequest "Merchant access already assigned."
  merchantAccess <- buildMerchantAccess personId merchant.id merchant.shortId req.operatingCity
  QAccess.create merchantAccess
  pure Success

resetMerchantAccess ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  TokenInfo ->
  Id DP.Person ->
  MerchantAccessReq ->
  m APISuccess
resetMerchantAccess _ personId req = do
  merchant <-
    QMerchant.findByShortId req.merchantId
      >>= fromMaybeM (MerchantDoesNotExist req.merchantId.getShortId)
  merchantServerAccessCheck merchant
  _person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchantAccesses <- QAccess.findByPersonIdAndMerchantId personId merchant.id
  case merchantAccesses of
    [] -> throwError $ InvalidRequest "Server access already denied."
    (x : _) -> do
      -- this function uses tokens from db, so should be called before transaction
      Auth.cleanCachedTokensByMerchantId personId merchant.id
      QAccess.deleteById x.id
      QReg.deleteAllByPersonIdAndMerchantId personId merchant.id
      pure Success

resetMerchantCityAccess ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  TokenInfo ->
  Id DP.Person ->
  MerchantCityAccessReq ->
  m APISuccess
resetMerchantCityAccess _ personId req = do
  merchant <-
    QMerchant.findByShortId req.merchantId
      >>= fromMaybeM (MerchantDoesNotExist req.merchantId.getShortId)
  merchantServerAccessCheck merchant
  _person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  mbMerchantAccess <- QAccess.findByPersonIdAndMerchantIdAndCity personId merchant.id req.operatingCity
  case mbMerchantAccess of
    Nothing -> throwError $ InvalidRequest "Server access already denied."
    Just merchantAccess -> do
      -- this function uses tokens from db, so should be called before transaction
      Auth.cleanCachedTokensByMerchantIdAndCity personId merchant.id req.operatingCity
      QAccess.deleteById merchantAccess.id
      QReg.deleteAllByPersonIdAndMerchantIdAndCity personId merchant.id req.operatingCity
      pure Success

changePassword ::
  ( BeamFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    HasFlowEnv m r '["enforceStrongPasswordPolicy" ::: Bool]
  ) =>
  TokenInfo ->
  ChangePasswordReq ->
  m APISuccess
changePassword tokenInfo req = do
  encPerson <- QP.findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
  enforceStrongPasswordPolicy <- asks (.enforceStrongPasswordPolicy)
  when enforceStrongPasswordPolicy $
    validateStrongPassword req.newPassword

  newHash <- getDbHash req.newPassword
  let oldActual = encPerson.passwordHash
  oldProvided <- getDbHash req.oldPassword
  unless (oldActual == Just oldProvided) . throwError $ InvalidRequest "Old password is incorrect."
  QP.updatePersonPassword tokenInfo.personId newHash
  -- Cleanup: delete auth token from cache and db to enforce re-authentication after password change
  Auth.cleanCachedTokensByMerchantIdAndCity tokenInfo.personId tokenInfo.merchantId tokenInfo.city
  QReg.deleteAllByPersonIdAndMerchantIdAndCity tokenInfo.personId tokenInfo.merchantId tokenInfo.city
  pure Success

changePasswordAfterExpiry ::
  (BeamFlow m r, EncFlow m r, HasFlowEnv m r '["enforceStrongPasswordPolicy" ::: Bool]) =>
  ChangePasswordAfterExpiryReq ->
  m APISuccess
changePasswordAfterExpiry req = do
  encPerson <- QP.findByEmailAndPassword req.email req.oldPassword >>= fromMaybeM (PersonDoesNotExist req.email)
  enforceStrongPasswordPolicy <- asks (.enforceStrongPasswordPolicy)
  when enforceStrongPasswordPolicy $
    validateStrongPassword req.newPassword
  newHash <- getDbHash req.newPassword
  QP.updatePersonPassword encPerson.id newHash
  pure Success

validateStrongPassword :: (BeamFlow m r) => Text -> m ()
validateStrongPassword password = do
  let pwd = T.unpack password
      specialChars :: [Char]
      specialChars = "!@#$%^&*()-_=+[]{}|;:',.<>?/`~"

  unless (length pwd >= 10) $
    throwError $ InvalidRequest "Password must be at least 10 characters long."

  unless (any isUpper pwd) $
    throwError $ InvalidRequest "Password must contain at least one uppercase letter."

  unless (any isLower pwd) $
    throwError $ InvalidRequest "Password must contain at least one lowercase letter."

  unless (any isDigit pwd) $
    throwError $ InvalidRequest "Password must contain at least one number."

  unless (any (`elem` specialChars) pwd) $
    throwError $ InvalidRequest "Password must contain at least one special character."

buildMerchantAccess :: MonadFlow m => Id DP.Person -> Id DMerchant.Merchant -> ShortId DMerchant.Merchant -> City.City -> m DAccess.MerchantAccess
buildMerchantAccess personId merchantId merchantShortId city = do
  uid <- generateGUID
  now <- getCurrentTime
  return $
    DAccess.MerchantAccess
      { id = Id uid,
        personId = personId,
        merchantId = merchantId,
        merchantShortId = merchantShortId,
        secretKey = Nothing,
        is2faEnabled = False,
        createdAt = now,
        operatingCity = city
      }

profile ::
  (BeamFlow m r, EncFlow m r) =>
  TokenInfo ->
  m DP.PersonAPIEntity
profile tokenInfo = do
  encPerson <- B.runInReplica $ QP.findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
  role <- B.runInReplica $ CQRole.findById encPerson.roleId >>= fromMaybeM (RoleNotFound encPerson.roleId.getId)
  merchantAccessList <- B.runInReplica $ QAccess.findAllMerchantAccessByPersonId tokenInfo.personId
  decPerson <- decrypt encPerson
  case merchantAccessList of
    [] -> throwError (InvalidRequest "No access to any merchant")
    merchantAccessList' -> do
      let sortedMerchantAccessList = sortOn DAccess.merchantId merchantAccessList'
      let groupedByMerchant = groupBy ((==) `on` DAccess.merchantId) sortedMerchantAccessList
      let merchantAccesslistWithCity = map (\group -> DP.AvailableCitiesForMerchant ((.merchantShortId) (head group)) (map (.operatingCity) group)) groupedByMerchant
      pure $ DP.makePersonAPIEntity decPerson role (merchantAccesslistWithCity <&> (.merchantShortId)) (Just merchantAccesslistWithCity)

getCurrentMerchant ::
  BeamFlow m r =>
  TokenInfo ->
  m MerchantAccessRes
getCurrentMerchant tokenInfo = do
  merchant <-
    B.runInReplica $
      QMerchant.findById tokenInfo.merchantId
        >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
  pure $ MerchantCityAccessReq merchant.shortId tokenInfo.city

getAccessMatrix ::
  BeamFlow m r =>
  TokenInfo ->
  m DMatrix.AccessMatrixRowAPIEntity
getAccessMatrix tokenInfo = do
  encPerson <- B.runInReplica $ QP.findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
  role <- B.runInReplica $ CQRole.findById encPerson.roleId >>= fromMaybeM (RoleNotFound encPerson.roleId.getId)
  accessMatrixItems <- B.runInReplica $ QMatrix.findAllByRoleId encPerson.roleId
  pure $ DMatrix.mkAccessMatrixRowAPIEntity accessMatrixItems role

changePasswordByAdmin ::
  (BeamFlow m r, EncFlow m r, HasFlowEnv m r '["enforceStrongPasswordPolicy" ::: Bool]) =>
  TokenInfo ->
  Id DP.Person ->
  ChangePasswordByAdminReq ->
  m APISuccess
changePasswordByAdmin _ personId req = do
  void $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  enforceStrongPasswordPolicy <- asks (.enforceStrongPasswordPolicy)
  when enforceStrongPasswordPolicy $
    validateStrongPassword req.newPassword
  newHash <- getDbHash req.newPassword
  QP.updatePersonPassword personId newHash
  pure Success

changeMobileNumberByAdmin ::
  (BeamFlow m r, EncFlow m r, HasFlowEnv m r '["updateRestrictedBppRoles" ::: [Text]]) =>
  TokenInfo ->
  Id DP.Person ->
  ChangeMobileNumberByAdminReq ->
  m APISuccess
changeMobileNumberByAdmin _ personId req = do
  runRequestValidation validateChangeMobileNumberReq req
  mobileDbHash <- getDbHash req.newMobileNumber
  result <- QP.findByIdWithRoleAndCheckMobileHash personId (Just mobileDbHash)
  let (mbPersonAndRole, isDuplicateNumber) = result
  unless (null isDuplicateNumber) $ throwError (InvalidRequest "Phone already registered")
  (_person, role) <- fromMaybeM (PersonNotFound personId.getId) mbPersonAndRole
  updateRestrictedBppRoles <- asks (.updateRestrictedBppRoles)
  when (role.name `elem` updateRestrictedBppRoles) $
    throwError $ InvalidRequest $ "Cannot update phone number for role: " <> role.name
  encMobileNum <- encrypt req.newMobileNumber
  QP.updatePersonMobile personId encMobileNum
  QPerson.updatePersonMobileByFleetRole personId.getId encMobileNum
  pure Success

changeEnabledStatus ::
  (BeamFlow m r, EncFlow m r, HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]) =>
  TokenInfo ->
  Id DP.Person ->
  ChangeEnabledStatusReq ->
  m APISuccess
changeEnabledStatus tokenInfo personId req = do
  void $ B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Auth.cleanCachedTokensByMerchantIdAndCity personId tokenInfo.merchantId tokenInfo.city
  QReg.updateEnabledStatusByPersonIdAndMerchantIdAndCity personId tokenInfo.merchantId tokenInfo.city req.enabled
  pure Success

changeEmailByAdmin ::
  (BeamFlow m r, EncFlow m r) =>
  TokenInfo ->
  Id DP.Person ->
  ChangeEmailByAdminReq ->
  m APISuccess
changeEmailByAdmin _ personId req = do
  void $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  encEmail <- encrypt $ T.toLower req.newEmail
  QP.updatePersonEmail personId encEmail
  pure Success

deletePerson ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  TokenInfo ->
  Id DP.Person ->
  m APISuccess
deletePerson _ personId = do
  void $ B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  QAccess.deleteAllByPersonId personId
  Auth.cleanCachedTokens personId
  QReg.deleteAllByPersonId personId
  QP.deletePerson personId
  pure Success

buildPerson :: (EncFlow m r) => CreatePersonReq -> DRole.DashboardAccessType -> m SP.Person
buildPerson req dashboardAccessType = do
  pid <- generateGUID
  now <- getCurrentTime
  mobileNumber <- encrypt req.mobileNumber
  --TODO write query to make existing email in person table to lower case
  email <- encrypt (T.toLower req.email)
  passwordHash <- getDbHash req.password
  return
    SP.Person
      { id = pid,
        firstName = req.firstName,
        lastName = req.lastName,
        roleId = req.roleId,
        email = Just email,
        mobileNumber = mobileNumber,
        mobileCountryCode = req.mobileCountryCode,
        passwordHash = Just passwordHash,
        dashboardAccessType = Just dashboardAccessType,
        dashboardType = fromMaybe DPT.DEFAULT_DASHBOARD req.dashboardType,
        receiveNotification = Nothing,
        createdAt = now,
        updatedAt = now,
        verified = Nothing,
        rejectionReason = Nothing,
        rejectedAt = Nothing,
        passwordUpdatedAt = Just now,
        approvedBy = Nothing,
        rejectedBy = Nothing
      }

data UpdatePersonReq = UpdatePersonReq
  { firstName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

updatePerson :: (BeamFlow m r, EncFlow m r) => Id SP.Person -> UpdatePersonReq -> m APISuccess
updatePerson personId req = do
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  encryptedEmail <- case req.email of
    Just email -> do
      emailExists <- B.runInReplica $ QP.findByEmail email
      when (isJust emailExists) $ throwError (InvalidRequest "Email already registered")
      res <- encrypt (T.toLower email)
      return $ Just res
    Nothing -> pure person.email
  encryptedMobileNumber <- case req.mobileNumber of
    Just mobileNumber -> do
      mobileNumberExists <- B.runInReplica $ QP.findByMobileNumber mobileNumber person.mobileCountryCode
      when (isJust mobileNumberExists) $ throwError (InvalidRequest "Phone already registered")
      encrypt mobileNumber
    Nothing -> pure person.mobileNumber
  let updatedPerson =
        person
          { SP.firstName = fromMaybe person.firstName req.firstName,
            SP.lastName = fromMaybe person.lastName req.lastName,
            SP.email = encryptedEmail,
            SP.mobileNumber = encryptedMobileNumber,
            SP.mobileCountryCode = fromMaybe person.mobileCountryCode req.mobileCountryCode
          }
  QP.updatePerson personId updatedPerson
  pure Success
