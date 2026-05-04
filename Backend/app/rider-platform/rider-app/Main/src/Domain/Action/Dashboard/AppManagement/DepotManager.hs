{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.DepotManager
  ( postDepotManagerUpsertOne,
    postDepotManagerUpsertMany,
    getDepotManagerList,
    deleteDepotManager,
    getDepotManagerByMobileNumber,
    getDepotManagerByDepotCode,
    getDepotManagerByPersonId,
  )
where

import qualified API.Types.Dashboard.AppManagement.DepotManager as API
import Data.Either (lefts, rights)
import qualified Domain.Types.Depot as DDepot
import qualified Domain.Types.DepotManager as DDepotManager
import Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (getDbHash)
import qualified Kernel.Types.APISuccess
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DepotManager as QDepotManager
import qualified Storage.Queries.DepotManagerExtra as QDepotManagerExtra
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonExtra as QP

postDepotManagerUpsertOne ::
  ShortId DM.Merchant ->
  Context.City ->
  API.DepotManagerDetail ->
  Flow API.UpsertDepotManagerResp
postDepotManagerUpsertOne merchantShortId opCity req = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)

  result <- withTryCatch "postDepotManagerUpsertOne" $ do
    QDepotManagerExtra.upsertDepotManagerDetail merchant merchantOperatingCity req

  case result of
    Left _err ->
      pure $
        API.UpsertDepotManagerResp
          { success = "Failed to upsert depot manager",
            succeededCount = 0,
            failedCount = 1,
            succeededPhoneNumbers = [],
            failedPhoneNumbers = [req.mobileNumber]
          }
    Right _ ->
      pure $
        API.UpsertDepotManagerResp
          { success = "Depot manager upserted successfully",
            succeededCount = 1,
            failedCount = 0,
            succeededPhoneNumbers = [req.mobileNumber],
            failedPhoneNumbers = []
          }

postDepotManagerUpsertMany ::
  ShortId DM.Merchant ->
  Context.City ->
  API.DepotManagerDetails ->
  Flow API.UpsertDepotManagerResp
postDepotManagerUpsertMany merchantShortId opCity req = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)

  results <- forM req.items $ \item -> do
    result <- withTryCatch "postDepotManagerUpsertMany" $ QDepotManagerExtra.upsertDepotManagerDetail merchant merchantOperatingCity item
    case result of
      Left _err -> pure (Left item.mobileNumber)
      Right _ -> pure (Right item.mobileNumber)

  let succeeded = rights results
      failed = lefts results
      successMsg = if null failed then "All depot managers upserted successfully" else "Some depot managers failed to upsert"

  pure $
    API.UpsertDepotManagerResp
      { success = successMsg,
        succeededCount = length succeeded,
        failedCount = length failed,
        succeededPhoneNumbers = succeeded,
        failedPhoneNumbers = failed
      }

getDepotManagerList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Flow API.DepotManagerListResp
getDepotManagerList merchantShortId _opCity mLimit mOffset = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let limit = fromMaybe 20 mLimit
      offset = fromMaybe 0 mOffset
  depotManagers <- QDepotManagerExtra.findAll merchant.id (Just limit) (Just offset)
  items <- forM depotManagers $ \dm -> do
    mbPerson <- QPerson.findById dm.personId
    let countryCode = fromMaybe "" $ mbPerson >>= (\p -> p.mobileCountryCode)
    pure $
      API.DepotManagerInfo
        { depotCode = dm.depotCode.getId,
          personId = dm.personId,
          countryCode = countryCode,
          isAdmin = dm.isAdmin,
          enabled = dm.enabled,
          createdAt = dm.createdAt,
          updatedAt = dm.updatedAt
        }
  pure $ API.DepotManagerListResp {items = items}

deleteDepotManager ::
  ShortId DM.Merchant ->
  Context.City ->
  API.DeleteDepotManagerReq ->
  Flow Kernel.Types.APISuccess.APISuccess
deleteDepotManager merchantShortId _opCity req = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let depotId = Id req.depotCode :: Id DDepot.Depot
  mbDepotManager <- QDepotManager.findByPrimaryKey depotId req.personId
  case mbDepotManager of
    Just depotManager
      | depotManager.merchantId /= merchant.id ->
        throwError $ InvalidRequest "Depot manager does not belong to this merchant"
      | otherwise -> do
        QDepotManagerExtra.deleteByPrimaryKey depotId req.personId
        pure Kernel.Types.APISuccess.Success
    Nothing -> throwError $ InvalidRequest "Depot manager not found"

getDepotManagerByMobileNumber ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Maybe Text ->
  Flow API.DepotManagerInfo
getDepotManagerByMobileNumber merchantShortId _opCity mbMobileNumber mbCountryCode = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  mobileNumber <- fromMaybeM (InvalidRequest "mobileNumber is required") mbMobileNumber
  countryCode <- fromMaybeM (InvalidRequest "countryCode is required") mbCountryCode
  mobileNumberHash <- getDbHash mobileNumber
  mbPerson <- QP.findByMobileNumberAndMerchantId countryCode mobileNumberHash merchant.id
  person <- fromMaybeM (PersonNotFound $ "Mobile number: " <> mobileNumber) mbPerson
  mbDepotManager <- QDepotManager.findByPersonId person.id
  case mbDepotManager of
    Just depotManager
      | depotManager.merchantId /= merchant.id ->
        throwError $ InvalidRequest "Depot manager does not belong to this merchant"
      | otherwise -> do
        pure $
          API.DepotManagerInfo
            { depotCode = depotManager.depotCode.getId,
              personId = depotManager.personId,
              countryCode = fromMaybe "" person.mobileCountryCode,
              isAdmin = depotManager.isAdmin,
              enabled = depotManager.enabled,
              createdAt = depotManager.createdAt,
              updatedAt = depotManager.updatedAt
            }
    Nothing -> throwError $ InvalidRequest "Depot manager not found for this mobile number"

getDepotManagerByDepotCode ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Flow API.DepotManagerListResp
getDepotManagerByDepotCode merchantShortId _opCity mbDepotCode mbLimit mbOffset = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  depotCodeStr <- fromMaybeM (InvalidRequest "depotCode is required") mbDepotCode
  let depotId = Id depotCodeStr :: Id DDepot.Depot
      limit = fromMaybe 20 mbLimit
      offset = fromMaybe 0 mbOffset
  depotManagers <- QDepotManagerExtra.findAllByDepotCode merchant.id depotId (Just limit) (Just offset)
  items <- forM depotManagers $ \dm -> do
    mbPerson <- QPerson.findById dm.personId
    let countryCode = fromMaybe "" $ mbPerson >>= (\p -> p.mobileCountryCode)
    pure $
      API.DepotManagerInfo
        { depotCode = dm.depotCode.getId,
          personId = dm.personId,
          countryCode = countryCode,
          isAdmin = dm.isAdmin,
          enabled = dm.enabled,
          createdAt = dm.createdAt,
          updatedAt = dm.updatedAt
        }
  pure $ API.DepotManagerListResp {items = items}

getDepotManagerByPersonId ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe (Id DP.Person) ->
  Flow API.DepotManagerInfo
getDepotManagerByPersonId merchantShortId _opCity mbPersonId = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  personId <- fromMaybeM (InvalidRequest "personId is required") mbPersonId
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbDepotManager <- QDepotManager.findByPersonId personId
  case mbDepotManager of
    Just depotManager
      | depotManager.merchantId /= merchant.id ->
        throwError $ InvalidRequest "Depot manager not found for this person"
      | otherwise -> do
        pure $
          API.DepotManagerInfo
            { depotCode = depotManager.depotCode.getId,
              personId = depotManager.personId,
              countryCode = fromMaybe "" person.mobileCountryCode,
              isAdmin = depotManager.isAdmin,
              enabled = depotManager.enabled,
              createdAt = depotManager.createdAt,
              updatedAt = depotManager.updatedAt
            }
    Nothing -> throwError $ InvalidRequest "Depot manager not found for this person"
