{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.DepotManager
  ( postDepotManagerUpsertOne,
    postDepotManagerUpsertMany,
  )
where

import qualified API.Types.Dashboard.AppManagement.DepotManager as APIDepotManager
import qualified Data.Text as T
import qualified Domain.Types.DepotManager
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (getDbHash)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Domain.Action.UI.Registration as Registration
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DepotManager as QDepotManager
import qualified Storage.Queries.PersonExtra as QPersonExtra

-- | Find person by phone number using hash lookup
findPersonByPhoneNumber ::
  Kernel.Prelude.Text ->
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Environment.Flow (Maybe Domain.Types.Person.Person)
findPersonByPhoneNumber phoneNumber merchantId = do
  -- Extract country code (assuming +91 or similar prefix)
  let (countryCode, mobileNumber) = splitPhoneNumber phoneNumber
  mobileNumberHash <- getDbHash mobileNumber
  QPersonExtra.findByMobileNumberAndMerchantId countryCode mobileNumberHash merchantId
  where
    splitPhoneNumber :: Text -> (Text, Text)
    splitPhoneNumber txt =
      case T.uncons txt of
        Just ('+', rest) ->
          let (code, num) = T.splitAt 2 rest
           in ("+" <> code, num)
        _ -> ("+91", txt) -- Default to +91 if no country code

-- | Find or create a person by phone number
findOrCreatePerson ::
  Kernel.Prelude.Text ->
  Kernel.Prelude.Text ->
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Environment.Flow Domain.Types.Person.Person
findOrCreatePerson countryCode mobileNumber merchantId = do
  mbPerson <- findPersonByPhoneNumber mobileNumber merchantId
  case mbPerson of
    Just person -> return person
    Nothing -> do
      personId <- Registration.createPersonWithPhoneNumber merchantId mobileNumber (Just countryCode)
      -- Fetch the newly created person
      QPersonExtra.findByPId personId >>= \case
        Just person -> return person
        Nothing -> throwError $ InternalError "Failed to fetch newly created person"

-- | Upsert a single depot manager entry
upsertDepotManager ::
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Kernel.Types.Beckn.Context.City ->
  APIDepotManager.DepotManagerDetail ->
  Environment.Flow (Either Text ())
upsertDepotManager merchantId merchantOperatingCityId _city detail = do
  person <- findOrCreatePerson detail.countryCode detail.mobileNumber merchantId
  let personId = person.id
      depotCodeId = Kernel.Types.Id.Id detail.depotCode
      isAdminValue = fromMaybe False detail.isAdmin

  now <- getCurrentTime

  -- Check if record already exists
  mbExisting <- QDepotManager.findByPrimaryKey depotCodeId personId

  case mbExisting of
    Just existing -> do
      -- Update existing record
      QDepotManager.updateByPrimaryKey
        Domain.Types.DepotManager.DepotManager
          { depotCode = depotCodeId,
            personId = personId,
            enabled = True,
            isAdmin = isAdminValue,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            createdAt = existing.createdAt,
            updatedAt = now
          }
      pure $ Right ()
    Nothing -> do
      -- Create new record
      QDepotManager.create
        Domain.Types.DepotManager.DepotManager
          { depotCode = depotCodeId,
            personId = personId,
            enabled = True,
            isAdmin = isAdminValue,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }
      pure $ Right ()

-- | Process multiple depot manager entries and collect results
processDepotManagers ::
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Kernel.Types.Beckn.Context.City ->
  [APIDepotManager.DepotManagerDetail] ->
  Environment.Flow ([Text], [Text])
processDepotManagers merchantId merchantOperatingCityId city details = do
  results <- forM details $ \detail -> do
    result <- upsertDepotManager merchantId merchantOperatingCityId city detail
    case result of
      Right _ -> pure (Right detail.mobileNumber)
      Left err -> do
        logError $ "Failed to upsert depot manager: " <> show err
        pure (Left detail.mobileNumber)

  let succeeded = rights results
      failed = lefts results

  pure (succeeded, failed)

-- | Upsert a single depot manager
postDepotManagerUpsertOne ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  APIDepotManager.DepotManagerDetail ->
  Environment.Flow APIDepotManager.UpsertDepotManagerResp
postDepotManagerUpsertOne merchantShortId city req = do
  merchantOpCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city)

  let merchantId = merchantOpCity.merchantId
      merchantOperatingCityId = merchantOpCity.id

  result <- upsertDepotManager merchantId merchantOperatingCityId city req

  case result of
    Right _ ->
      pure $
        APIDepotManager.UpsertDepotManagerResp
          { success = "Depot manager upserted successfully",
            succeededCount = 1,
            failedCount = 0,
            succeededPhoneNumbers = [req.mobileNumber],
            failedPhoneNumbers = []
          }
    Left _ ->
      pure $
        APIDepotManager.UpsertDepotManagerResp
          { success = "Failed to upsert depot manager",
            succeededCount = 0,
            failedCount = 1,
            succeededPhoneNumbers = [],
            failedPhoneNumbers = [req.mobileNumber]
          }

-- | Upsert multiple depot managers in bulk
postDepotManagerUpsertMany ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  APIDepotManager.DepotManagerDetails ->
  Environment.Flow APIDepotManager.UpsertDepotManagerResp
postDepotManagerUpsertMany merchantShortId city req = do
  merchantOpCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city)

  let merchantId = merchantOpCity.merchantId
      merchantOperatingCityId = merchantOpCity.id

  (succeeded, failed) <- processDepotManagers merchantId merchantOperatingCityId city req.items

  let successMsg = case (length succeeded, length failed) of
        (s, 0) -> "All " <> show s <> " depot manager(s) upserted successfully"
        (0, f) -> "All " <> show f <> " depot manager(s) failed to upsert"
        (s, f) -> show s <> " succeeded, " <> show f <> " failed"

  pure $
    APIDepotManager.UpsertDepotManagerResp
      { success = successMsg,
        succeededCount = length succeeded,
        failedCount = length failed,
        succeededPhoneNumbers = succeeded,
        failedPhoneNumbers = failed
      }
