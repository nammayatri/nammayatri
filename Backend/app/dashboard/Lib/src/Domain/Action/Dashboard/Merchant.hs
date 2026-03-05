{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Merchant where

import Dashboard.Common
import qualified Data.Text as T
import qualified Domain.Action.Dashboard.Person as DPerson
import Domain.Types.Merchant
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.API as AP
import qualified Domain.Types.Person.Type as SP
import qualified Domain.Types.Role as DRole
import qualified Domain.Types.ServerName as DTServer
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.MerchantAccess as QMerchantAccess
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QRT
import qualified Storage.Queries.Role as QRole
import Tools.Auth
import qualified Tools.Auth.Common as Auth
import Tools.Error

data CreateMerchantWithAdminReq = CreateMerchantWithAdminReq
  { shortId :: Text,
    is2faMandatory :: Bool,
    defaultOperatingCity :: City.City,
    supportedOperatingCities :: [City.City],
    domain :: Text,
    website :: Text,
    adminEmail :: Text,
    adminPassword :: Text,
    adminFirstName :: Text,
    adminLastName :: Text,
    adminMobileCountryCode :: Text,
    adminMobileNumber :: Text,
    dashboardType :: Maybe SP.DashboardType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CreateMerchantReq = CreateMerchantReq
  { shortId :: Text,
    is2faMandatory :: Bool,
    defaultOperatingCity :: City.City,
    supportedOperatingCities :: [City.City],
    domain :: Text,
    website :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ListMerchantResp = ListMerchantResp
  { list :: [DMerchant.MerchantAPIEntity],
    summary :: Summary
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ChangeMerchantEnableStateReq = ChangeMerchantEnableStateReq
  { enable :: Bool,
    merchantShortId :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

createMerchantWithAdmin ::
  (BeamFlow m r, EncFlow m r, HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]]) =>
  TokenInfo ->
  CreateMerchantWithAdminReq ->
  m DP.PersonAPIEntity
createMerchantWithAdmin tokenInfo req = do
  let shortId = ShortId req.shortId :: ShortId DMerchant.Merchant
  mbExistingMerchant <- QMerchant.findByShortId shortId
  whenJust mbExistingMerchant $ \_ -> throwError (MerchantAlreadyExist req.shortId)
  unlessM (isNothing <$> QP.findByEmail req.adminEmail) $ throwError (InvalidRequest "Email already registered")
  unlessM (isNothing <$> QP.findByMobileNumber req.adminMobileNumber req.adminMobileCountryCode) $ throwError (InvalidRequest "Phone already registered")
  role <- QRole.findByName "MERCHANT_ADMIN" >>= fromMaybeM (RoleDoesNotExist "MERCHANT_ADMIN")
  person <- buildPersonCreateReq req role
  decPerson <- decrypt person
  QP.create person
  merchant <- buildMerchant CreateMerchantReq {shortId = req.shortId, is2faMandatory = req.is2faMandatory, defaultOperatingCity = req.defaultOperatingCity, supportedOperatingCities = req.supportedOperatingCities, domain = req.domain, website = req.website}
  QMerchant.create merchant
  merchantAccess <- DPerson.buildMerchantAccess person.id merchant.id merchant.shortId tokenInfo.city
  QAccess.create merchantAccess
  pure $ AP.makePersonAPIEntity decPerson role [merchant.shortId] (Just [DP.AvailableCitiesForMerchant {merchantShortId = merchant.shortId, operatingCity = [req.defaultOperatingCity]}])

createMerchant ::
  (BeamFlow m r, EncFlow m r) =>
  TokenInfo ->
  CreateMerchantReq ->
  m DMerchant.MerchantAPIEntity
createMerchant _ req = do
  let shortId = ShortId req.shortId :: ShortId DMerchant.Merchant
  mbExistingMerchant <- QMerchant.findByShortId shortId
  whenJust mbExistingMerchant $ \_ -> throwError (MerchantAlreadyExist req.shortId)
  merchant <- buildMerchant req
  decMerchant <- decrypt merchant
  QMerchant.create merchant
  pure $ DMerchant.mkMerchantAPIEntity decMerchant []

buildMerchant ::
  (BeamFlow m r, EncFlow m r) =>
  CreateMerchantReq ->
  m DMerchant.Merchant
buildMerchant req = do
  uid <- generateGUID
  now <- getCurrentTime
  authToken <- generateGUID
  encryptedAuthToken <- encrypt authToken
  pure
    DMerchant.Merchant
      { id = uid,
        shortId = ShortId req.shortId :: ShortId DMerchant.Merchant,
        serverNames = [],
        is2faMandatory = req.is2faMandatory,
        defaultOperatingCity = req.defaultOperatingCity,
        supportedOperatingCities = req.supportedOperatingCities,
        domain = Just req.domain,
        website = Just req.website,
        authToken = Just encryptedAuthToken,
        enabled = Just True,
        createdAt = now,
        requireAdminApprovalForFleetOnboarding = Just False,
        verifyFleetWhileLogin = Just True,
        hasFleetMemberHierarchy = Just True,
        isStrongNameCheckRequired = Just True,
        singleActiveSessionOnly = Just False,
        enableGetRequestAuditLogs = Just False
      }

changeMerchantEnableState ::
  (BeamFlow m r, EncFlow m r, HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]) =>
  TokenInfo ->
  ChangeMerchantEnableStateReq ->
  m APISuccess
changeMerchantEnableState tokenInfo req = do
  let shortId = ShortId req.merchantShortId :: ShortId DMerchant.Merchant
  merchant <- QMerchant.findByShortId shortId >>= fromMaybeM (MerchantNotFound req.merchantShortId)
  when (merchant.enabled == Just req.enable) $ throwError (InvalidRequest "Merchant already in the requested state")
  Auth.cleanCachedTokensOfMerchantAndCity merchant.id tokenInfo.city
  QRT.deleteAllByMerchantIdAndCity merchant.id tokenInfo.city
  QMerchant.updateEnableStatus merchant.shortId req.enable
  pure Success

listMerchants ::
  (BeamFlow m r, EncFlow m r) =>
  TokenInfo ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  m ListMerchantResp
listMerchants _ mbLimit mbOffset mbShortId = do
  let limit = fromMaybe 10 mbLimit
  let offset = fromMaybe 0 mbOffset
  merchantList <- case mbShortId of
    Just shortId -> do
      merchant <-
        QMerchant.findByShortId
          (ShortId shortId)
      case merchant of
        Just m -> pure [m]
        Nothing -> pure []
    Nothing -> QMerchant.findAllMerchants' limit offset
  list <-
    forM merchantList $ \merchant -> do
      decryptedMerchant <- decrypt merchant
      allMerchantUsers <- QMerchantAccess.findAllUserAccountForMerchant decryptedMerchant.id
      let personIds = map (\x -> x.personId) allMerchantUsers
      allPersons <- QP.findAllByIds personIds
      decryptedPersons <- mapM (\person -> decrypt person) allPersons
      let emailList = map (\person -> (fromMaybe person.mobileNumber person.email, person.id.getId)) decryptedPersons
      pure $ DMerchant.mkMerchantAPIEntity decryptedMerchant emailList
  pure ListMerchantResp {list = list, summary = Summary {totalCount = 10000, count = length list}}

createUserForMerchant ::
  (BeamFlow m r, EncFlow m r, HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]], HasFlowEnv m r '["merchantUserAccountNumber" ::: Int]) =>
  TokenInfo ->
  DPerson.CreatePersonReq ->
  m DPerson.CreatePersonRes
createUserForMerchant tokenInfo req = do
  merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
  merchantAssociatedAccount <- QMerchantAccess.findAllUserAccountForMerchant merchant.id
  merchantUserAccountLimit <- asks (.merchantUserAccountNumber)
  when (length merchantAssociatedAccount >= merchantUserAccountLimit) $ throwError (MerchantAccountLimitExceeded (merchant.shortId.getShortId))
  personEntity <- DPerson.createPerson tokenInfo req
  _ <- DPerson.assignMerchantCityAccess tokenInfo personEntity.person.id DPerson.MerchantCityAccessReq {operatingCity = tokenInfo.city, merchantId = merchant.shortId}
  pure personEntity

buildPersonCreateReq :: (BeamFlow m r, EncFlow m r) => CreateMerchantWithAdminReq -> DRole.Role -> m SP.Person
buildPersonCreateReq req role = do
  pid <- generateGUID
  now <- getCurrentTime
  mobileNumber <- encrypt req.adminMobileNumber
  email <- encrypt (T.toLower req.adminEmail)
  passwordHash <- getDbHash req.adminPassword
  return
    SP.Person
      { id = pid,
        firstName = req.adminFirstName,
        lastName = req.adminLastName,
        email = Just email,
        passwordHash = Just passwordHash,
        mobileCountryCode = req.adminMobileCountryCode,
        mobileNumber = mobileNumber,
        roleId = role.id,
        dashboardAccessType = Just role.dashboardAccessType,
        verified = Nothing,
        receiveNotification = Nothing,
        createdAt = now,
        updatedAt = now,
        rejectionReason = Nothing,
        rejectedAt = Nothing,
        dashboardType = fromMaybe SP.DEFAULT_DASHBOARD req.dashboardType,
        passwordUpdatedAt = Just now,
        approvedBy = Nothing,
        rejectedBy = Nothing
      }
