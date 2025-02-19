{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Customer
  ( deleteCustomerDelete,
    postCustomerBlock,
    postCustomerUnblock,
    getCustomerList,
    getCustomerInfo,
    postCustomerCancellationDuesSync,
    getCustomerCancellationDuesDetails,
    postCustomerUpdateSafetyCenterBlocking,
    postCustomerPersonNumbers,
    postCustomerPersonId,
  )
where

import qualified "dashboard-helper-api" API.Types.RiderPlatform.Management.Customer as Common
import qualified Dashboard.Common as Common
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import Data.List.Split (chunksOf)
import qualified Data.Vector as V
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude
import Kernel.Storage.Hedis (withCrossAppRedis)
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.MerchantConfig as SMC
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.MerchantConfig as CMC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Clickhouse.Sos as CHSos
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.SafetySettings as QSafety
import qualified Storage.Queries.SavedReqLocation as QSRL

---------------------------------------------------------------------
deleteCustomerDelete ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Customer ->
  Flow APISuccess
deleteCustomerDelete merchantShortId opCity customerId = do
  let personId = cast @Common.Customer @DP.Person customerId
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  person <- runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound $ getId personId)
  unless (merchant.id == person.merchantId && person.merchantOperatingCityId == merchantOpCity.id) $ throwError (PersonDoesNotExist $ getId personId)
  bookings <- runInReplica $ QRB.findByRiderIdAndStatus personId [DRB.NEW, DRB.TRIP_ASSIGNED, DRB.AWAITING_REASSIGNMENT, DRB.CONFIRMED, DRB.COMPLETED]
  unless (null bookings) $ throwError (InvalidRequest "Can't delete customer, has a valid booking in past.")
  _ <- QP.deleteById personId
  QPFS.clearCache personId
  _ <- QSRL.deleteAllByRiderId personId
  pure Success

---------------------------------------------------------------------
postCustomerBlock :: ShortId DM.Merchant -> Context.City -> Id Common.Customer -> Flow APISuccess
postCustomerBlock merchantShortId opCity customerId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  let personId = cast @Common.Customer @DP.Person customerId
  customer <-
    runInReplica $
      QP.findById personId
        >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = customer.merchantId
  unless (merchant.id == merchantId && customer.merchantOperatingCityId == merchantOpCity.id) $ throwError (PersonDoesNotExist personId.getId)

  SMC.blockCustomer personId Nothing
  logTagInfo "dashboard -> blockCustomer : " (show personId)
  pure Success

---------------------------------------------------------------------
postCustomerUnblock :: ShortId DM.Merchant -> Context.City -> Id Common.Customer -> Flow APISuccess
postCustomerUnblock merchantShortId opCity customerId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  let personId = cast @Common.Customer @DP.Person customerId
  customer <-
    runInReplica $
      QP.findById personId
        >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = customer.merchantId
  unless (merchant.id == merchantId && customer.merchantOperatingCityId == merchantOpCity.id) $ throwError (PersonDoesNotExist personId.getId)
  merchantConfigs <- CMC.findAllByMerchantOperatingCityId customer.merchantOperatingCityId Nothing
  mapM_
    ( \mc -> withCrossAppRedis $ do
        SWC.deleteCurrentWindowValues (SMC.mkCancellationKey mc.id.getId personId.getId) mc.fraudBookingCancellationCountWindow
        SWC.deleteCurrentWindowValues (SMC.mkCancellationByDriverKey mc.id.getId personId.getId) mc.fraudBookingCancelledByDriverCountWindow
    )
    merchantConfigs
  void $ QP.updatingEnabledAndBlockedState personId Nothing False
  logTagInfo "dashboard -> unblockCustomer : " (show personId)
  pure Success

---------------------------------------------------------------------
getCustomerInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Customer -> Flow Common.CustomerInfoRes
getCustomerInfo merchantShortId opCity customerId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  let personId = cast @Common.Customer @DP.Person customerId
  customer <-
    runInReplica $
      QP.findById personId
        >>= fromMaybeM (PersonDoesNotExist personId.getId)
  safetySettings <- QSafety.findSafetySettingsWithFallback personId Nothing
  -- merchant access checking
  let merchantId = customer.merchantId
  unless (merchant.id == merchantId && customer.merchantOperatingCityId == merchantOpCity.id) $ throwError (PersonDoesNotExist personId.getId)

  numberOfRides <- fromMaybe 0 <$> runInReplica (QP.fetchRidesCount personId)
  sos <- CHSos.findAllByPersonId personId
  let totalSosCount = length sos
  pure $
    Common.CustomerInfoRes
      { numberOfRides,
        falseSafetyAlarmCount = safetySettings.falseSafetyAlarmCount,
        safetyCenterDisabledOnDate = safetySettings.safetyCenterDisabledOnDate,
        ..
      }

---------------------------------------------------------------------
getCustomerList :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe (Id Common.Customer) -> Flow Common.CustomerListRes
getCustomerList merchantShortId opCity mbLimit mbOffset mbEnabled mbBlocked mbSearchPhone mbPersonId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
      offset = fromMaybe 0 mbOffset
  mbSearchPhoneDBHash <- getDbHash `traverse` mbSearchPhone
  customers <- runInReplica $ QP.findAllCustomers merchant merchantOpCity limit offset mbEnabled mbBlocked mbSearchPhoneDBHash (cast @Common.Customer @DP.Person <$> mbPersonId)
  items <- mapM buildCustomerListItem customers
  let count = length items
  let summary = Common.Summary {totalCount = 10000, count}
  pure Common.CustomerListRes {totalItems = count, summary, customers = items}
  where
    maxLimit = 20
    defaultLimit = 10

buildCustomerListItem :: EncFlow m r => DP.Person -> m Common.CustomerListItem
buildCustomerListItem person = do
  phoneNo <- mapM decrypt person.mobileNumber
  pure $
    Common.CustomerListItem
      { customerId = cast @DP.Person @Common.Customer person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        phoneNo,
        enabled = person.enabled,
        blocked = person.blocked
      }

---------------------------------------------------------------------
postCustomerCancellationDuesSync ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Customer ->
  Common.CustomerCancellationDuesSyncReq ->
  Flow APISuccess
postCustomerCancellationDuesSync merchantShortId _ customerId req = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let personId = cast @Common.Customer @DP.Person customerId
  person <- runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mobNum <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  void $ CallBPPInternal.customerCancellationDuesSync merchant.driverOfferApiKey merchant.driverOfferBaseUrl merchant.driverOfferMerchantId mobNum (fromMaybe "+91" person.mobileCountryCode) req.cancellationCharges req.cancellationChargesWithCurrency req.disputeChancesUsed req.paymentMadeToDriver person.currentCity
  return Success

getCustomerCancellationDuesDetails ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Customer ->
  Flow Common.CancellationDuesDetailsRes
getCustomerCancellationDuesDetails merchantShortId _ personId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  res <- DCancel.getCancellationDuesDetails Nothing (cast personId, merchant.id)
  return $
    Common.CancellationDuesDetailsRes
      { cancellationDues = res.cancellationDues,
        disputeChancesUsed = res.disputeChancesUsed,
        canBlockCustomer = res.canBlockCustomer
      }

postCustomerUpdateSafetyCenterBlocking ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Customer ->
  Common.UpdateSafetyCenterBlockingReq ->
  Flow APISuccess
postCustomerUpdateSafetyCenterBlocking _ _ personId req = do
  let personId' = cast @Common.Customer @DP.Person personId
  safetySettings <- QSafety.findSafetySettingsWithFallback personId' Nothing
  case req.resetCount of
    Just True -> do
      QSafety.updateSafetyCenterBlockingCounter personId' (Just 0) Nothing
    _ -> pure ()
  case req.incrementCount of
    Just True -> do
      now <- getCurrentTime
      QSafety.updateSafetyCenterBlockingCounter personId' (Just $ safetySettings.falseSafetyAlarmCount + 1) $ blockingDate now safetySettings.falseSafetyAlarmCount
    _ -> pure ()
  return Success
  where
    blockingDate now count = if (count + 1) == 3 || count + 1 >= 6 then Just now else Nothing

postCustomerPersonNumbers :: ShortId DM.Merchant -> Context.City -> Common.PersonIdsReq -> Flow [Common.PersonRes]
postCustomerPersonNumbers _ _ req = do
  csvData <- readCsvAndGetPersonIds req.file
  let chunks = chunksOf 100 csvData
  decryptedNumbers <- forM chunks processChunk
  return $ concat decryptedNumbers
  where
    readCsvAndGetPersonIds :: FilePath -> Flow [Text]
    readCsvAndGetPersonIds csvFile = do
      csvData <- liftIO $ BS.readFile csvFile
      case decodeByName (LBS.fromStrict csvData) of
        Left err -> throwError (InvalidRequest $ show err)
        Right (_, v) -> pure $ map Common.personId $ V.toList v

    processChunk :: [Text] -> Flow [Common.PersonRes]
    processChunk chunk = do
      persons <- QP.findAllByPersonIds chunk
      decryptedPersons <- forM persons $ \p -> do
        decPerson <- decrypt p
        return $ Common.PersonRes decPerson.id.getId decPerson.mobileNumber Nothing decPerson.merchantOperatingCityId.getId
      return decryptedPersons

postCustomerPersonId :: ShortId DM.Merchant -> Context.City -> Common.PersonMobileNoReq -> Flow [Common.PersonRes]
postCustomerPersonId _ _ req = do
  csvData <- readCsvAndGetPersonIds req.file
  let chunks = chunksOf 100 csvData
  decryptedNumbers <- forM chunks processChunk
  return $ concat decryptedNumbers
  where
    readCsvAndGetPersonIds :: FilePath -> Flow [Text]
    readCsvAndGetPersonIds csvFile = do
      csvData <- liftIO $ BS.readFile csvFile
      case decodeByName (LBS.fromStrict csvData) of
        Left err -> throwError (InvalidRequest $ show err)
        Right (_, v) -> pure $ mapMaybe (Common.mobileNumber :: Common.PersonMobileNumberIdsCsvRow -> Maybe Text) $ V.toList v

    processChunk :: [Text] -> Flow [Common.PersonRes]
    processChunk chunk = do
      mobile <- QP.findPersonIdsByPhoneNumber chunk
      decryptedPersons <- forM mobile $ \p -> do
        decMobile <- decrypt p
        return $ Common.PersonRes decMobile.id.getId decMobile.mobileNumber Nothing decMobile.merchantOperatingCityId.getId
      return decryptedPersons
