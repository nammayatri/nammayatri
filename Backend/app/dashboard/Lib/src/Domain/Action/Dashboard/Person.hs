{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Person where

import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantAccess as DAccess
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.API as AP
import qualified Domain.Types.Person.Type as SP
import qualified Domain.Types.Role as DRole
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QReg
import qualified Storage.Queries.Role as QRole
import Tools.Auth
import qualified Tools.Auth.Common as Auth
import qualified Tools.Client as Client
import Tools.Error

newtype ListPersonRes = ListPersonRes
  {list :: [DP.PersonAPIEntity]}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype MerchantAccessReq = MerchantAccessReq
  {merchantId :: ShortId DMerchant.Merchant}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type MerchantAccessRes = MerchantAccessReq

data ChangePasswordReq = ChangePasswordReq
  { oldPassword :: Text,
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
    password :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

validateCreatePerson :: Validate CreatePersonReq
validateCreatePerson CreatePersonReq {..} =
  sequenceA_
    [ validateField "firstName" firstName $ MinLength 3 `And` P.name,
      validateField "lastName" lastName $ NotEmpty `And` P.name,
      validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileIndianCode
    ]

newtype CreatePersonRes = CreatePersonRes
  {person :: AP.PersonAPIEntity}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

createPerson ::
  forall m r.
  (EsqDBFlow m r, EncFlow m r) =>
  TokenInfo ->
  CreatePersonReq ->
  m CreatePersonRes
createPerson _ personEntity = do
  runRequestValidation validateCreatePerson personEntity
  unlessM (isNothing <$> QP.findByEmail personEntity.email (Proxy @m)) $ throwError (InvalidRequest "Email already registered")
  unlessM (isNothing <$> QP.findByMobileNumber personEntity.mobileNumber personEntity.mobileCountryCode (Proxy @m)) $ throwError (InvalidRequest "Phone already registered")
  person <- buildPerson personEntity
  decPerson <- decrypt person
  let roleId = personEntity.roleId
  role <- QRole.findById roleId (Proxy @m) >>= fromMaybeM (RoleDoesNotExist roleId.getId)
  let personAPIEntity = AP.makePersonAPIEntity decPerson role []
  Esq.runTransaction $ QP.create @m person
  return $ CreatePersonRes personAPIEntity

listPerson ::
  forall m r.
  (EsqDBReplicaFlow m r, EncFlow m r) =>
  TokenInfo ->
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  m ListPersonRes
listPerson _ mbSearchString mbLimit mbOffset = do
  mbSearchStrDBHash <- getDbHash `traverse` mbSearchString
  personAndRoleList <- runInReplica $ QP.findAllWithLimitOffset mbSearchString mbSearchStrDBHash mbLimit mbOffset (Proxy @m)
  res <- forM personAndRoleList $ \(encPerson, role, merchantAccessList) -> do
    decPerson <- decrypt encPerson
    pure $ DP.makePersonAPIEntity decPerson role merchantAccessList
  pure $ ListPersonRes res

assignRole ::
  forall m r.
  EsqDBFlow m r =>
  TokenInfo ->
  Id DP.Person ->
  Id DRole.Role ->
  m APISuccess
assignRole _ personId roleId = do
  _person <- QP.findById personId (Proxy @m) >>= fromMaybeM (PersonDoesNotExist personId.getId)
  _role <- QRole.findById roleId (Proxy @m) >>= fromMaybeM (RoleDoesNotExist roleId.getId)
  Esq.runTransaction $
    QP.updatePersonRole @m personId roleId
  pure Success

assignMerchantAccess ::
  forall m r.
  ( EsqDBFlow m r,
    HasFlowEnv m r '["dataServers" ::: [Client.DataServer]]
  ) =>
  TokenInfo ->
  Id DP.Person ->
  MerchantAccessReq ->
  m APISuccess
assignMerchantAccess _ personId req = do
  merchant <-
    QMerchant.findByShortId req.merchantId (Proxy @m)
      >>= fromMaybeM (MerchantDoesNotExist req.merchantId.getShortId)
  availableServers <- asks (.dataServers)
  unless (merchant.serverName `elem` (availableServers <&> (.name))) $
    throwError $ InvalidRequest "Server for this merchant is not available"
  _person <- QP.findById personId (Proxy @m) >>= fromMaybeM (PersonDoesNotExist personId.getId)
  mbMerchantAccess <- QAccess.findByPersonIdAndMerchantId personId merchant.id (Proxy @m)
  whenJust mbMerchantAccess $ \_ -> do
    throwError $ InvalidRequest "Merchant access already assigned."
  merchantAccess <- buildMerchantAccess personId merchant.id
  Esq.runTransaction $
    QAccess.create @m merchantAccess
  pure Success

resetMerchantAccess ::
  forall m r.
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["dataServers" ::: [Client.DataServer]],
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  TokenInfo ->
  Id DP.Person ->
  MerchantAccessReq ->
  m APISuccess
resetMerchantAccess _ personId req = do
  merchant <-
    QMerchant.findByShortId req.merchantId (Proxy @m)
      >>= fromMaybeM (MerchantDoesNotExist req.merchantId.getShortId)
  availableServers <- asks (.dataServers)
  unless (merchant.serverName `elem` (availableServers <&> (.name))) $
    throwError $ InvalidRequest "Server for this merchant is not available"
  _person <- QP.findById personId (Proxy @m) >>= fromMaybeM (PersonDoesNotExist personId.getId)
  mbMerchantAccess <- QAccess.findByPersonIdAndMerchantId personId merchant.id (Proxy @m)
  case mbMerchantAccess of
    Nothing -> throwError $ InvalidRequest "Server access already denied."
    Just merchantAccess -> do
      -- this function uses tokens from db, so should be called before transaction
      Auth.cleanCachedTokensByMerchantId personId merchant.id
      Esq.runTransaction $ do
        QAccess.deleteById @m merchantAccess.id
        QReg.deleteAllByPersonIdAndMerchantId personId merchant.id
      pure Success

changePassword ::
  forall m r.
  (EsqDBFlow m r, EncFlow m r) =>
  TokenInfo ->
  ChangePasswordReq ->
  m APISuccess
changePassword tokenInfo req = do
  encPerson <- QP.findById tokenInfo.personId (Proxy @m) >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
  newHash <- getDbHash req.newPassword
  let oldActual = encPerson.passwordHash
  oldProvided <- getDbHash req.oldPassword
  unless (oldActual == oldProvided) . throwError $ InvalidRequest "Old password is incorrect."
  Esq.runTransaction $
    QP.updatePersonPassword @m tokenInfo.personId newHash
  pure Success

buildMerchantAccess :: MonadFlow m => Id DP.Person -> Id DMerchant.Merchant -> m DAccess.MerchantAccess
buildMerchantAccess personId merchantId = do
  uid <- generateGUID
  now <- getCurrentTime
  return $
    DAccess.MerchantAccess
      { id = Id uid,
        personId = personId,
        merchantId = merchantId,
        createdAt = now
      }

profile ::
  forall m r.
  (EsqDBReplicaFlow m r, EncFlow m r) =>
  TokenInfo ->
  m DP.PersonAPIEntity
profile tokenInfo = do
  encPerson <- runInReplica $ QP.findById tokenInfo.personId (Proxy @m) >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
  role <- runInReplica $ QRole.findById encPerson.roleId (Proxy @m) >>= fromMaybeM (RoleNotFound encPerson.roleId.getId)
  merchantAccessList <- runInReplica $ QAccess.findAllByPersonId tokenInfo.personId (Proxy @m)
  decPerson <- decrypt encPerson
  pure $ DP.makePersonAPIEntity decPerson role (merchantAccessList <&> (.shortId))

getCurrentMerchant ::
  forall m r.
  EsqDBReplicaFlow m r =>
  TokenInfo ->
  m MerchantAccessRes
getCurrentMerchant tokenInfo = do
  merchant <-
    runInReplica $
      QMerchant.findById tokenInfo.merchantId (Proxy @m)
        >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
  pure $ MerchantAccessReq merchant.shortId

buildPerson :: (EncFlow m r) => CreatePersonReq -> m SP.Person
buildPerson req = do
  pid <- generateGUID
  now <- getCurrentTime
  mobileNumber <- encrypt req.mobileNumber
  email <- encrypt req.email
  passwordHash <- getDbHash req.password
  return
    SP.Person
      { id = pid,
        firstName = req.firstName,
        lastName = req.lastName,
        roleId = req.roleId,
        email = email,
        mobileNumber = mobileNumber,
        mobileCountryCode = req.mobileCountryCode,
        passwordHash = passwordHash,
        createdAt = now,
        updatedAt = now
      }
