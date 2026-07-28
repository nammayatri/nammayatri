module Domain.Action.Dashboard.AppManagement.EDCMachine
  ( assignEDCMachine,
    listEDCMachine,
    updateEDCMachine,
    deleteEDCMachine,
  )
where

import qualified API.Types.Dashboard.AppManagement.EDCMachine as API
import qualified Domain.Types.EDCMachineMapping as DEDCM
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.EDCMachineMapping as QEDCMachineMapping
import qualified Storage.Queries.EDCMachineMappingExtra as QEDCMachineMappingExtra

-- | Assign an EDC machine to a person.
-- If the person already has an active mapping, or the machine is already assigned,
-- the old mapping(s) are deactivated before creating the new one.
assignEDCMachine ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.AssignEDCMachineReq ->
  Environment.Flow API.AssignEDCMachineResp
assignEDCMachine merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let merchantId = merchant.id
      merchantOperatingCityId = merchantOperatingCity.id

  -- Deactivate any existing active mapping for the target person
  QEDCMachineMappingExtra.deactivateExistingMapping req.personId merchantId merchantOperatingCityId

  -- Deactivate any existing active mapping for this machine (if assigned to someone else)
  QEDCMachineMappingExtra.deactivateByTerminalId req.paytmTid merchantId merchantOperatingCityId

  -- Create new mapping
  newId <- generateGUID
  now <- getCurrentTime
  let mapping =
        DEDCM.EDCMachineMapping
          { id = newId,
            personId = req.personId,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            terminalId = req.paytmTid,
            merchantKey = req.merchantKey,
            merchantChannelId = req.channelId,
            paytmMid = req.paytmMid,
            clientId = req.clientId,
            machineName = req.machineName,
            isActive = True,
            createdAt = now,
            updatedAt = now,
            createdBy = Kernel.Prelude.Nothing
          }
  QEDCMachineMapping.create mapping
  pure $
    API.AssignEDCMachineResp
      { id = newId,
        personId = req.personId,
        machineName = req.machineName,
        paytmTid = req.paytmTid,
        isActive = True
      }

-- | List EDC machine mappings scoped to the merchant/city with optional filters.
listEDCMachine ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Bool ->
  Kernel.Prelude.Maybe (Id.Id DP.Person) ->
  Environment.Flow API.EDCMachineMappingListResp
listEDCMachine merchantShortId opCity mbIsActive mbPersonId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  mappings <- QEDCMachineMappingExtra.findAllByMerchantAndCityWithFilters merchant.id merchantOperatingCity.id mbPersonId mbIsActive
  pure $ API.EDCMachineMappingListResp {mappings = map mkListItem mappings}
  where
    mkListItem mapping =
      API.EDCMachineMappingListItem
        { id = mapping.id,
          personId = mapping.personId,
          machineName = mapping.machineName,
          terminalId = mapping.terminalId,
          isActive = mapping.isActive,
          createdAt = mapping.createdAt
        }

-- | Update an EDC machine mapping.
updateEDCMachine ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id.Id DEDCM.EDCMachineMapping ->
  API.UpdateEDCMachineReq ->
  Environment.Flow APISuccess.APISuccess
updateEDCMachine _merchantShortId _opCity mappingId req = do
  mapping <- QEDCMachineMapping.findById mappingId >>= fromMaybeM (InvalidRequest "EDC machine mapping not found")
  now <- getCurrentTime
  let newTerminalId = fromMaybe mapping.terminalId req.paytmTid
      isReactivating = req.isActive == Kernel.Prelude.Just True && not mapping.isActive
  -- Mirror assign: reactivating a mapping must not leave two active mappings
  -- for the same person or the same terminal.
  when isReactivating $ do
    QEDCMachineMappingExtra.deactivateExistingMapping mapping.personId mapping.merchantId mapping.merchantOperatingCityId
    QEDCMachineMappingExtra.deactivateByTerminalId newTerminalId mapping.merchantId mapping.merchantOperatingCityId
  let updatedMapping =
        mapping
          { DEDCM.machineName = req.machineName <|> mapping.machineName,
            DEDCM.paytmMid = fromMaybe mapping.paytmMid req.paytmMid,
            DEDCM.merchantChannelId = fromMaybe mapping.merchantChannelId req.channelId,
            DEDCM.clientId = fromMaybe mapping.clientId req.clientId,
            DEDCM.merchantKey = fromMaybe mapping.merchantKey req.merchantKey,
            DEDCM.terminalId = newTerminalId,
            DEDCM.isActive = fromMaybe mapping.isActive req.isActive,
            DEDCM.updatedAt = now
          }
  QEDCMachineMapping.updateByPrimaryKey updatedMapping
  pure APISuccess.Success

-- | Soft-delete an EDC machine mapping (set isActive = false).
deleteEDCMachine ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id.Id DEDCM.EDCMachineMapping ->
  Environment.Flow APISuccess.APISuccess
deleteEDCMachine _merchantShortId _opCity mappingId = do
  mapping <- QEDCMachineMapping.findById mappingId >>= fromMaybeM (InvalidRequest "EDC machine mapping not found")
  now <- getCurrentTime
  let deactivatedMapping = mapping {DEDCM.isActive = False, DEDCM.updatedAt = now}
  QEDCMachineMapping.updateByPrimaryKey deactivatedMapping
  pure APISuccess.Success
