{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.EDCMachine
  ( postEdcMachineAssign,
    getEdcMachineList,
    putEdcMachineUpdate,
    deleteEdcMachineDelete,
  )
where

import qualified API.Types.UI.EDCMachine as API
import qualified Domain.Types.EDCMachineMapping as DEDCM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import qualified Storage.Queries.EDCMachineMapping as QEDCMachineMapping
import qualified Storage.Queries.EDCMachineMappingExtra as QEDCMachineMappingExtra
import qualified Storage.Queries.Person as QP

-- | Assign an EDC machine to a user.
-- If the user already has an active mapping, deactivate the old one and create a new one.
postEdcMachineAssign ::
  ( ( Kernel.Prelude.Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    API.AssignEDCMachineReq ->
    Flow API.AssignEDCMachineResp
  )
postEdcMachineAssign (mbRequestorId, merchantId) req = do
  requestorId <- mbRequestorId & fromMaybeM (InvalidRequest "Person not found")
  person <- QP.findById requestorId >>= fromMaybeM (InvalidRequest "Person not found")
  let merchantOperatingCityId = person.merchantOperatingCityId

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
            createdBy = Just requestorId
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

-- | List EDC machine mappings with optional filters.
getEdcMachineList ::
  ( ( Kernel.Prelude.Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Kernel.Prelude.Maybe (Id.Id DP.Person) ->
    Kernel.Prelude.Maybe Bool ->
    Flow API.EDCMachineMappingListResp
  )
getEdcMachineList (mbRequestorId, merchantId) mbPersonId mbIsActive = do
  requestorId <- mbRequestorId & fromMaybeM (InvalidRequest "Person not found")
  person <- QP.findById requestorId >>= fromMaybeM (InvalidRequest "Person not found")
  let merchantOperatingCityId = person.merchantOperatingCityId
  mappings <- QEDCMachineMappingExtra.findAllByMerchantAndCityWithFilters merchantId merchantOperatingCityId mbPersonId mbIsActive
  let items = map mkListItem mappings
  pure $ API.EDCMachineMappingListResp {mappings = items}
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
putEdcMachineUpdate ::
  ( ( Kernel.Prelude.Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Id.Id DEDCM.EDCMachineMapping ->
    API.UpdateEDCMachineReq ->
    Flow APISuccess.APISuccess
  )
putEdcMachineUpdate (mbRequestorId, _merchantId) mappingId req = do
  _requestorId <- mbRequestorId & fromMaybeM (InvalidRequest "Person not found")
  mapping <- QEDCMachineMapping.findById mappingId >>= fromMaybeM (InvalidRequest "EDC machine mapping not found")
  now <- getCurrentTime
  let updatedMapping =
        mapping
          { DEDCM.machineName = req.machineName <|> mapping.machineName,
            DEDCM.paytmMid = fromMaybe mapping.paytmMid req.paytmMid,
            DEDCM.merchantChannelId = fromMaybe mapping.merchantChannelId req.channelId,
            DEDCM.clientId = fromMaybe mapping.clientId req.clientId,
            DEDCM.merchantKey = fromMaybe mapping.merchantKey req.merchantKey,
            DEDCM.terminalId = fromMaybe mapping.terminalId req.paytmTid,
            DEDCM.isActive = fromMaybe mapping.isActive req.isActive,
            DEDCM.updatedAt = now
          }
  QEDCMachineMapping.updateByPrimaryKey updatedMapping
  pure APISuccess.Success

-- | Soft-delete an EDC machine mapping (set isActive = false).
deleteEdcMachineDelete ::
  ( ( Kernel.Prelude.Maybe (Id.Id DP.Person),
      Id.Id DM.Merchant
    ) ->
    Id.Id DEDCM.EDCMachineMapping ->
    Flow APISuccess.APISuccess
  )
deleteEdcMachineDelete (mbRequestorId, _merchantId) mappingId = do
  _requestorId <- mbRequestorId & fromMaybeM (InvalidRequest "Person not found")
  mapping <- QEDCMachineMapping.findById mappingId >>= fromMaybeM (InvalidRequest "EDC machine mapping not found")
  now <- getCurrentTime
  let deactivatedMapping = mapping {DEDCM.isActive = False, DEDCM.updatedAt = now}
  QEDCMachineMapping.updateByPrimaryKey deactivatedMapping
  pure APISuccess.Success
