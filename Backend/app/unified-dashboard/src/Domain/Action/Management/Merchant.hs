{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Management.Merchant
  ( createMerchantWithAdmin,
    createMerchant,
    listMerchants,
    changeMerchantEnableState,
  )
where

import qualified API.Types.Management.Merchant
import qualified API.Types.Management.Person
import Data.List (nub)
import qualified Data.Text
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantAccess as DAccess
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Role as DRole
import qualified Domain.Types.ServerName
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, encrypt, encrypted, getDbHash, unEncrypted)
import qualified Kernel.External.Verification.SafetyPortal.Types
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as City
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.MerchantExtra as QMerchantExtra
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Role as QRole
import Tools.Auth.Api
import Tools.Auth.Common as Auth
import Tools.Auth.Merchant
import Tools.Error

createMerchantWithAdmin ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  API.Types.Management.Merchant.CreateMerchantWithAdminReq ->
  Environment.Flow API.Types.Management.Merchant.PersonAPIEntity
createMerchantWithAdmin _ _ apiTokenInfo req = do
  let shortId = Kernel.Types.Id.ShortId req.shortId :: Kernel.Types.Id.ShortId DMerchant.Merchant
  mbExistingMerchant <- QMerchant.findByShortId shortId
  whenJust mbExistingMerchant $ \_ -> throwError (InvalidRequest $ "Merchant with shortId " <> req.shortId <> " already exists")

  emailHash <- getDbHash (T.toLower req.adminEmail)
  mbExistingPerson <- QPerson.findByEmailHash (Just emailHash)
  whenJust mbExistingPerson $ \_ -> throwError (InvalidRequest "Email already registered")

  mobileHash <- getDbHash req.adminMobileNumber
  mbExistingPersonByMobile <- QPerson.findByMobileNumberHash (Just mobileHash) (Just req.adminMobileCountryCode)
  whenJust mbExistingPersonByMobile $ \_ -> throwError (InvalidRequest "Phone already registered")

  role <- QRole.findByName "MERCHANT_ADMIN" >>= fromMaybeM (InvalidRequest "Role MERCHANT_ADMIN does not exist")

  -- Create person
  personId <- generateGUID
  now <- getCurrentTime
  encryptedEmail <- encrypt (T.toLower req.adminEmail)
  encryptedMobileNumber <- encrypt req.adminMobileNumber
  passwordHash <- getDbHash req.adminPassword

  let person =
        DPerson.Person
          { id = personId,
            firstName = req.adminFirstName,
            lastName = req.adminLastName,
            roleId = role.id,
            email = Just encryptedEmail,
            mobileNumber = Just encryptedMobileNumber,
            mobileCountryCode = Just req.adminMobileCountryCode,
            passwordHash = Just passwordHash,
            createdAt = now,
            updatedAt = now,
            receiveNotification = Nothing,
            verified = Nothing,
            rejectionReason = Nothing,
            rejectedAt = Nothing,
            passwordUpdatedAt = now,
            approvedBy = Nothing,
            rejectedBy = Nothing
          }
  QPerson.create person

  -- Create merchant
  let merchantReq =
        API.Types.Management.Merchant.CreateMerchantReq
          { shortId = req.shortId,
            is2faMandatory = req.is2faMandatory,
            defaultOperatingCity = req.defaultOperatingCity,
            supportedOperatingCities = req.supportedOperatingCities,
            domain = req.domain,
            website = req.website
          }
  merchant <- buildMerchant merchantReq
  QMerchant.create merchant

  -- Create merchant access
  merchantAccess <- buildMerchantAccess person.id merchant.id merchant.shortId apiTokenInfo.city
  QAccess.create merchantAccess

  -- Build response
  decPerson <- decrypt person
  let availableCities = [DMerchant.AvailableCitiesForMerchant merchant.shortId [req.defaultOperatingCity]]
  pure $ mkPersonAPIEntity decPerson role [merchant.shortId] (Just availableCities)

createMerchant ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  API.Types.Management.Merchant.CreateMerchantReq ->
  Environment.Flow API.Types.Management.Merchant.MerchantAPIEntity
createMerchant _ _ _ req = do
  let shortId = Kernel.Types.Id.ShortId req.shortId :: Kernel.Types.Id.ShortId DMerchant.Merchant
  mbExistingMerchant <- QMerchant.findByShortId shortId
  whenJust mbExistingMerchant $ \_ -> throwError (InvalidRequest $ "Merchant with shortId " <> req.shortId <> " already exists")

  merchant <- buildMerchant req
  QMerchant.create merchant
  decMerchant <- decrypt merchant
  pure $ mkMerchantAPIEntity decMerchant []

listMerchants ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Data.Text.Text ->
  Environment.Flow API.Types.Management.Merchant.ListMerchantResp
listMerchants _ _ _ mbLimit mbOffset mbShortId = do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  merchantList <- case mbShortId of
    Just shortId -> do
      merchant <- QMerchant.findByShortId (Kernel.Types.Id.ShortId shortId)
      case merchant of
        Just m -> pure [m]
        Nothing -> pure []
    Nothing -> QMerchantExtra.findAllMerchants' limit offset

  list <-
    forM merchantList $ \merchant -> do
      decryptedMerchant <- decrypt merchant
      allMerchantUsers <- QAccess.findAllUserAccountForMerchant decryptedMerchant.id
      let personIds = map (.personId) allMerchantUsers
      allPersons <- B.runInReplica $ forM personIds QPerson.findById >>= pure . catMaybes
      decryptedPersons <- mapM decrypt allPersons
      let emailList = map (\person -> (fromMaybe "" $ person.email <|> person.mobileNumber, person.id.getId)) decryptedPersons
      pure $ mkMerchantAPIEntity decryptedMerchant emailList

  let summary = Kernel.External.Verification.SafetyPortal.Types.Summary {totalCount = 10000, count = length list}
  pure $ API.Types.Management.Merchant.ListMerchantResp {list = list, summary = summary}

changeMerchantEnableState ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  API.Types.Management.Merchant.ChangeMerchantEnableStateReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
changeMerchantEnableState _ _opCity _ req = do
  let shortId = Kernel.Types.Id.ShortId req.merchantShortId :: Kernel.Types.Id.ShortId DMerchant.Merchant
  merchant <- QMerchant.findByShortId shortId >>= fromMaybeM (InvalidRequest $ "Merchant with shortId " <> req.merchantShortId <> " not found")
  when (merchant.enabled == Just req.enable) $ throwError (InvalidRequest "Merchant already in the requested state")

  -- Invalidate tokens and cached data for all operating cities of the merchant
  let allOperatingCities = merchant.defaultOperatingCity : merchant.supportedOperatingCities
  forM_ (nub allOperatingCities) $ \city -> do
    Auth.cleanCachedTokensOfMerchantAndCity merchant.id city
    QR.deleteAllByMerchantIdAndCity merchant.id city

  QMerchant.updateEnableStatus (Just req.enable) merchant.shortId
  pure Kernel.Types.APISuccess.Success

-- Helper functions

buildMerchant ::
  (BeamFlow m r, EncFlow m r) =>
  API.Types.Management.Merchant.CreateMerchantReq ->
  m DMerchant.Merchant
buildMerchant req = do
  uid <- generateGUID
  now <- getCurrentTime
  authToken <- generateGUID
  encryptedAuthToken <- encrypt authToken
  pure
    DMerchant.Merchant
      { id = uid,
        shortId = Kernel.Types.Id.ShortId req.shortId :: Kernel.Types.Id.ShortId DMerchant.Merchant,
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
        updatedAt = now,
        enableGetRequestAuditLogs = Just False
      }

buildMerchantAccess ::
  MonadFlow m =>
  Kernel.Types.Id.Id DPerson.Person ->
  Kernel.Types.Id.Id DMerchant.Merchant ->
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  m DAccess.MerchantAccess
buildMerchantAccess personId merchantId merchantShortId city = do
  uid <- generateGUID
  now <- getCurrentTime
  return $
    DAccess.MerchantAccess
      { id = Kernel.Types.Id.Id uid,
        personId = personId,
        merchantId = merchantId,
        merchantShortId = merchantShortId,
        secretKey = Nothing,
        is2faEnabled = False,
        createdAt = now,
        operatingCity = city,
        updatedAt = now
      }

mkMerchantAPIEntity ::
  DMerchant.DecryptedMerchant ->
  [(Text, Text)] ->
  API.Types.Management.Merchant.MerchantAPIEntity
mkMerchantAPIEntity merchant adminList =
  API.Types.Management.Merchant.MerchantAPIEntity
    { id = merchant.id,
      shortId = merchant.shortId,
      domain = merchant.domain,
      website = merchant.website,
      authToken = merchant.authToken,
      supportedOperatingCities = merchant.supportedOperatingCities,
      defaultOperatingCity = merchant.defaultOperatingCity,
      adminList = adminList,
      enabled = merchant.enabled
    }

mkPersonAPIEntity ::
  DPerson.DecryptedPerson ->
  DRole.Role ->
  [Kernel.Types.Id.ShortId DMerchant.Merchant] ->
  Maybe [DMerchant.AvailableCitiesForMerchant] ->
  API.Types.Management.Merchant.PersonAPIEntity
mkPersonAPIEntity person role availableMerchants availableCitiesForMerchant =
  API.Types.Management.Merchant.PersonAPIEntity
    { id = person.id,
      firstName = person.firstName,
      lastName = person.lastName,
      role = mkRoleAPIEntity role,
      email = person.email,
      mobileNumber = fromMaybe "" person.mobileNumber,
      mobileCountryCode = fromMaybe "" person.mobileCountryCode,
      availableMerchants = availableMerchants,
      availableCitiesForMerchant = availableCitiesForMerchant,
      registeredAt = person.createdAt
    }

mkRoleAPIEntity :: DRole.Role -> DRole.RoleAPIEntity
mkRoleAPIEntity role =
  DRole.RoleAPIEntity
    { id = role.id,
      name = role.name,
      description = role.description,
      needsBppAccountCreation = role.needsBppAccountCreation
    }
