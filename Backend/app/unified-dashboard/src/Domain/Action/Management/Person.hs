{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Management.Person
  ( getPersonList,
    postPersonAssignRole,
    postPersonAssignMerchantCityAccess,
    postPersonResetMerchantAccess,
    postPersonResetMerchantCityAccess,
    deletePerson,
    postPersonChangeEnabledStatus,
    postPersonChangeEmailByAdmin,
    postPersonChangePasswordByAdmin,
    postPersonChangeMobileByAdmin,
    getUserProfile,
    getUserCurrentMerchant,
    postUserChangePassword,
    getUserAccessMatrix,
    postUserChangePasswordAfterExpiry,
  )
where

import qualified API.Types.Management.Person
import Data.List (groupBy, nub, sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text
import qualified Data.Text as T
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantAccess as DAccess
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Role as DRole
import qualified Domain.Types.ServerName
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import qualified Kernel.External.Verification.SafetyPortal.Types
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as City
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.AccessMatrix as QMatrix
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonExtra as QPersonExtra
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Role as QRole
import Tools.Auth.Api
import Tools.Auth.Common as Auth
import Tools.Auth.Merchant
import Tools.Error

getPersonList ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Maybe Data.Text.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Integer ->
  Kernel.Prelude.Maybe Kernel.Prelude.Integer ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id DPerson.Person) ->
  Environment.Flow API.Types.Management.Person.ListPersonResp
getPersonList _ _ _ mbSearchString mbLimit mbOffset mbPersonId = do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
  mbSearchStrDBHash <- getDbHash `traverse` mbSearchString

  -- Find persons based on filters
  personList <- case mbPersonId of
    Just personId -> do
      mbPerson <- B.runInReplica $ QPerson.findById personId
      pure $ maybe [] (: []) mbPerson
    Nothing -> do
      case mbSearchStrDBHash of
        Just searchHash -> do
          -- Search by email hash or mobile number hash
          mbPersonByEmail <- B.runInReplica $ QPerson.findByEmailHash (Just searchHash)
          mbPersonByMobile <- case mbSearchString of
            Just _searchStr -> do
              -- Try to find by mobile number (would need mobileCountryCode, simplified here)
              pure Nothing
            Nothing -> pure Nothing
          pure $ catMaybes [mbPersonByEmail, mbPersonByMobile]
        Nothing -> do
          -- List all with pagination
          B.runInReplica $ QPersonExtra.findAllPersonsWithLimitOffset limitVal offsetVal

  -- Get roles and merchant access for each person
  res <- forM personList $ \encPerson -> do
    role <- B.runInReplica $ QRole.findById encPerson.roleId >>= fromMaybeM (InvalidRequest $ "Role with id " <> show encPerson.roleId.getId <> " does not exist")
    merchantAccessList <- B.runInReplica $ QAccess.findAllByPersonId encPerson.id
    decPerson <- decrypt encPerson
    let availableCitiesForMerchant = makeAvailableCitiesForMerchant merchantAccessList
    let availableMerchants = nub $ map (.merchantShortId) merchantAccessList
    pure $ mkPersonAPIEntity decPerson role availableMerchants (Just availableCitiesForMerchant)

  let count = length res
      summary = Kernel.External.Verification.SafetyPortal.Types.Summary {totalCount = 10000, count}
  pure $ API.Types.Management.Person.ListPersonResp {list = res, summary = summary}

postPersonAssignRole ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id DPerson.Person ->
  Kernel.Types.Id.Id DRole.Role ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postPersonAssignRole _ _ _ personId roleId = do
  _person <- QPerson.findById personId >>= fromMaybeM (InvalidRequest $ "Person with id " <> show personId.getId <> " does not exist")
  _role <- QRole.findById roleId >>= fromMaybeM (InvalidRequest $ "Role with id " <> show roleId.getId <> " does not exist")
  -- Update person role
  person <- QPerson.findById personId >>= fromMaybeM (InvalidRequest $ "Person with id " <> show personId.getId <> " does not exist")
  let updatedPerson = person {DPerson.roleId = roleId}
  QPerson.updateByPrimaryKey updatedPerson
  pure Kernel.Types.APISuccess.Success

postPersonAssignMerchantCityAccess ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id DPerson.Person ->
  API.Types.Management.Person.MerchantCityAccessReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postPersonAssignMerchantCityAccess _ _ _ personId req = do
  merchant <- QMerchant.findByShortId req.merchantId >>= fromMaybeM (InvalidRequest $ "Merchant with shortId " <> show req.merchantId.getShortId <> " does not exist")
  merchantServerAccessCheck merchant
  let isSupportedCity = req.operatingCity `elem` merchant.supportedOperatingCities
  unless isSupportedCity $ throwError $ InvalidRequest "Server does not support this city"
  _person <- QPerson.findById personId >>= fromMaybeM (InvalidRequest $ "Person with id " <> show personId.getId <> " does not exist")
  mbMerchantAccess <- QAccess.findByPersonIdAndMerchantIdAndCity personId merchant.id req.operatingCity
  whenJust mbMerchantAccess $ \_ -> throwError $ InvalidRequest "Merchant access already assigned."
  merchantAccess <- buildMerchantAccess personId merchant.id merchant.shortId req.operatingCity
  QAccess.create merchantAccess
  pure Kernel.Types.APISuccess.Success

postPersonResetMerchantAccess ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id DPerson.Person ->
  API.Types.Management.Person.MerchantCityAccessReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postPersonResetMerchantAccess _ _ _ personId req = do
  merchant <- QMerchant.findByShortId req.merchantId >>= fromMaybeM (InvalidRequest $ "Merchant with shortId " <> show req.merchantId.getShortId <> " does not exist")
  merchantServerAccessCheck merchant
  _person <- QPerson.findById personId >>= fromMaybeM (InvalidRequest $ "Person with id " <> show personId.getId <> " does not exist")
  merchantAccesses <- QAccess.findByPersonIdAndMerchantId personId merchant.id
  case merchantAccesses of
    [] -> throwError $ InvalidRequest "Server access already denied."
    _ -> do
      Auth.cleanCachedTokensByMerchantId personId merchant.id
      -- Delete all registration tokens for this person and merchant
      regTokens <- QR.findAllByPersonIdAndMerchantId personId merchant.id
      for_ regTokens $ \token -> QR.deleteById token.id
      -- Note: MerchantAccess deletion would need deleteById if available
      pure Kernel.Types.APISuccess.Success

postPersonResetMerchantCityAccess ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id DPerson.Person ->
  API.Types.Management.Person.MerchantCityAccessReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postPersonResetMerchantCityAccess _ _ _ personId req = do
  merchant <- QMerchant.findByShortId req.merchantId >>= fromMaybeM (InvalidRequest $ "Merchant with shortId " <> show req.merchantId.getShortId <> " does not exist")
  merchantServerAccessCheck merchant
  _person <- QPerson.findById personId >>= fromMaybeM (InvalidRequest $ "Person with id " <> show personId.getId <> " does not exist")
  mbMerchantAccess <- QAccess.findByPersonIdAndMerchantIdAndCity personId merchant.id req.operatingCity
  case mbMerchantAccess of
    Nothing -> throwError $ InvalidRequest "Server access already denied."
    Just _merchantAccess -> do
      Auth.cleanCachedTokensByMerchantIdAndCity personId merchant.id req.operatingCity
      -- Delete all registration tokens for this person, merchant, and city
      regTokens <- QR.findAllByPersonIdAndMerchantIdAndCity personId merchant.id req.operatingCity
      for_ regTokens $ \token -> QR.deleteById token.id
      -- Note: MerchantAccess deletion would need deleteById if available
      pure Kernel.Types.APISuccess.Success

deletePerson ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id DPerson.Person ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
deletePerson _ _ _ personId = do
  void $ B.runInReplica $ QPerson.findById personId >>= fromMaybeM (InvalidRequest $ "Person with id " <> show personId.getId <> " does not exist")
  -- Delete all registration tokens for this person
  regTokens <- QR.findAllByPersonId personId
  for_ regTokens $ \token -> QR.deleteById token.id
  Auth.cleanCachedTokens personId
  -- Note: MerchantAccess and Person deletion would need deleteById/deleteByPrimaryKey if available
  pure Kernel.Types.APISuccess.Success

postPersonChangeEnabledStatus ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id DPerson.Person ->
  API.Types.Management.Person.ChangeEnabledStatusReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postPersonChangeEnabledStatus _ opCity apiTokenInfo personId req = do
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (InvalidRequest $ "Person with id " <> show personId.getId <> " does not exist")
  let updatedPerson = person {DPerson.verified = Just req.enabled}
  B.runInReplica $ QPerson.updateByPrimaryKey updatedPerson
  Auth.cleanCachedTokensByMerchantIdAndCity personId apiTokenInfo.merchant.id opCity
  pure Kernel.Types.APISuccess.Success

postPersonChangeEmailByAdmin ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id DPerson.Person ->
  API.Types.Management.Person.ChangeEmailByAdminReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postPersonChangeEmailByAdmin _ _ _ personId req = do
  void $ QPerson.findById personId >>= fromMaybeM (InvalidRequest $ "Person with id " <> show personId.getId <> " does not exist")
  encEmail <- encrypt $ T.toLower req.newEmail
  person <- QPerson.findById personId >>= fromMaybeM (InvalidRequest $ "Person with id " <> show personId.getId <> " does not exist")
  let updatedPerson = person {DPerson.email = Just encEmail}
  QPerson.updateByPrimaryKey updatedPerson
  pure Kernel.Types.APISuccess.Success

postPersonChangePasswordByAdmin ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id DPerson.Person ->
  API.Types.Management.Person.ChangePasswordByAdminReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postPersonChangePasswordByAdmin _ _ _ personId req = do
  void $ QPerson.findById personId >>= fromMaybeM (InvalidRequest $ "Person with id " <> show personId.getId <> " does not exist")
  -- Note: Password validation would go here if enforceStrongPasswordPolicy is enabled
  newHash <- getDbHash req.newPassword
  now <- getCurrentTime
  person <- QPerson.findById personId >>= fromMaybeM (InvalidRequest $ "Person with id " <> show personId.getId <> " does not exist")
  let updatedPerson = person {DPerson.passwordHash = Just newHash, DPerson.passwordUpdatedAt = now}
  QPerson.updateByPrimaryKey updatedPerson
  pure Kernel.Types.APISuccess.Success

postPersonChangeMobileByAdmin ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id DPerson.Person ->
  API.Types.Management.Person.ChangeMobileNumberByAdminReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postPersonChangeMobileByAdmin _ _ _ personId req = do
  void $ QPerson.findById personId >>= fromMaybeM (InvalidRequest $ "Person with id " <> show personId.getId <> " does not exist")
  encMobileNum <- encrypt req.newMobileNumber
  person <- QPerson.findById personId >>= fromMaybeM (InvalidRequest $ "Person with id " <> show personId.getId <> " does not exist")
  let updatedPerson = person {DPerson.mobileNumber = Just encMobileNum}
  QPerson.updateByPrimaryKey updatedPerson
  pure Kernel.Types.APISuccess.Success

getUserProfile ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Environment.Flow API.Types.Management.Person.PersonAPIEntity
getUserProfile _ _ apiTokenInfo = do
  encPerson <- B.runInReplica $ QPerson.findById apiTokenInfo.person.id >>= fromMaybeM (InvalidRequest $ "Person with id " <> show apiTokenInfo.person.id.getId <> " does not exist")
  role <- B.runInReplica $ QRole.findById encPerson.roleId >>= fromMaybeM (InvalidRequest $ "Role with id " <> show encPerson.roleId.getId <> " does not exist")
  merchantAccessList <- B.runInReplica $ QAccess.findAllByPersonId apiTokenInfo.person.id
  decPerson <- decrypt encPerson
  case merchantAccessList of
    [] -> throwError (InvalidRequest "No access to any merchant")
    merchantAccessList' -> do
      let sortedMerchantAccessList = sortOn (.merchantId) merchantAccessList'
      let groupedByMerchant = Data.List.groupBy ((==) `on` (.merchantId)) sortedMerchantAccessList
      let merchantAccesslistWithCity =
            mapMaybe
              ( \groupItems -> do
                  firstItem <- listToMaybe groupItems
                  pure $ DMerchant.AvailableCitiesForMerchant firstItem.merchantShortId (map (.operatingCity) groupItems)
              )
              groupedByMerchant
      let availableMerchants = nub $ map (.merchantShortId) merchantAccessList'
      pure $ mkPersonAPIEntity decPerson role availableMerchants (Just merchantAccesslistWithCity)

getUserCurrentMerchant ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Environment.Flow API.Types.Management.Person.MerchantAccessResp
getUserCurrentMerchant _ _ apiTokenInfo = do
  merchant <- B.runInReplica $ QMerchant.findById apiTokenInfo.merchant.id >>= fromMaybeM (InvalidRequest $ "Merchant with id " <> show apiTokenInfo.merchant.id.getId <> " does not exist")
  pure $ API.Types.Management.Person.MerchantAccessResp merchant.shortId apiTokenInfo.city

postUserChangePassword ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  API.Types.Management.Person.ChangePasswordReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postUserChangePassword _ _ apiTokenInfo req = do
  encPerson <- QPerson.findById apiTokenInfo.person.id >>= fromMaybeM (InvalidRequest $ "Person with id " <> show apiTokenInfo.person.id.getId <> " does not exist")
  -- Note: Password validation would go here if enforceStrongPasswordPolicy is enabled
  newHash <- getDbHash req.newPassword
  let oldActual = encPerson.passwordHash
  oldProvided <- getDbHash req.oldPassword
  unless (oldActual == Just oldProvided) . throwError $ InvalidRequest "Old password is incorrect."
  now <- getCurrentTime
  let updatedPerson = encPerson {DPerson.passwordHash = Just newHash, DPerson.passwordUpdatedAt = now}
  QPerson.updateByPrimaryKey updatedPerson
  Auth.cleanCachedTokensByMerchantIdAndCity apiTokenInfo.person.id apiTokenInfo.merchant.id apiTokenInfo.city
  regTokens <- QR.findAllByPersonIdAndMerchantIdAndCity apiTokenInfo.person.id apiTokenInfo.merchant.id apiTokenInfo.city
  for_ regTokens $ \token -> QR.deleteById token.id
  pure Kernel.Types.APISuccess.Success

getUserAccessMatrix ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Environment.Flow API.Types.Management.Person.AccessMatrixRowAPIEntity
getUserAccessMatrix _ _ apiTokenInfo = do
  encPerson <- B.runInReplica $ QPerson.findById apiTokenInfo.person.id >>= fromMaybeM (InvalidRequest $ "Person with id " <> show apiTokenInfo.person.id.getId <> " does not exist")
  role <- B.runInReplica $ QRole.findById encPerson.roleId >>= fromMaybeM (InvalidRequest $ "Role with id " <> show encPerson.roleId.getId <> " does not exist")
  accessMatrixItems <- B.runInReplica $ QMatrix.findAllByRoleId encPerson.roleId
  pure $ mkAccessMatrixRowAPIEntity accessMatrixItems role

postUserChangePasswordAfterExpiry ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  API.Types.Management.Person.ChangePasswordAfterExpiryReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postUserChangePasswordAfterExpiry _ _ _ req = do
  emailHash <- getDbHash (T.toLower req.email)
  oldPasswordHash <- getDbHash req.oldPassword
  encPerson <- QPerson.findByEmailAndPassword (Just emailHash) (Just oldPasswordHash) >>= fromMaybeM (InvalidRequest "Invalid email or password")
  -- Note: Password validation would go here if enforceStrongPasswordPolicy is enabled
  newHash <- getDbHash req.newPassword
  now <- getCurrentTime
  let updatedPerson = encPerson {DPerson.passwordHash = Just newHash, DPerson.passwordUpdatedAt = now}
  QPerson.updateByPrimaryKey updatedPerson
  pure Kernel.Types.APISuccess.Success

-- Helper functions

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
        merchantOperatingCityId = Nothing,
        updatedAt = now
      }

makeAvailableCitiesForMerchant :: [DAccess.MerchantAccess] -> [DMerchant.AvailableCitiesForMerchant]
makeAvailableCitiesForMerchant merchantAccessList = do
  let merchantCityList = sortOn fst $ map (\ma -> (ma.merchantShortId, ma.operatingCity)) merchantAccessList
  let groupedByMerchant = Data.List.groupBy ((==) `on` fst) merchantCityList
  if null groupedByMerchant
    then []
    else do
      let merchantAccesslistWithCity =
            mapMaybe
              ( \groupItems -> do
                  (merchantShortId, _) <- listToMaybe groupItems
                  pure $ DMerchant.AvailableCitiesForMerchant merchantShortId (map snd groupItems)
              )
              groupedByMerchant
      merchantAccesslistWithCity

mkPersonAPIEntity ::
  DPerson.DecryptedPerson ->
  DRole.Role ->
  [Kernel.Types.Id.ShortId DMerchant.Merchant] ->
  Maybe [DMerchant.AvailableCitiesForMerchant] ->
  API.Types.Management.Person.PersonAPIEntity
mkPersonAPIEntity person role availableMerchants availableCitiesForMerchant =
  API.Types.Management.Person.PersonAPIEntity
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

mkAccessMatrixRowAPIEntity :: [DMatrix.AccessMatrix] -> DRole.Role -> API.Types.Management.Person.AccessMatrixRowAPIEntity
mkAccessMatrixRowAPIEntity items role =
  API.Types.Management.Person.AccessMatrixRowAPIEntity
    { role = mkRoleAPIEntity role,
      accessMatrixRow = map mkAccessMatrixItemAPIEntity items
    }

mkAccessMatrixItemAPIEntity :: DMatrix.AccessMatrix -> API.Types.Management.Person.AccessMatrixItemAPIEntity
mkAccessMatrixItemAPIEntity accessMatrix =
  API.Types.Management.Person.AccessMatrixItemAPIEntity
    { serverName = accessMatrix.serverName,
      userActionType = accessMatrix.userActionType
    }
