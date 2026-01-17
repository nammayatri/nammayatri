module ExternalBPP.CallAPI.Verify where

import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import qualified Domain.Action.Beckn.FRFS.OnStatus as DOnStatus
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Environment
import qualified ExternalBPP.CallAPI.Status as CallExternalBPP
import qualified ExternalBPP.Flow as Flow
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Tools.Error

verifyTicket :: Id Merchant -> MerchantOperatingCity -> BecknConfig -> Spec.VehicleCategory -> Text -> DIBC.PlatformType -> Flow DFRFSTicket.FRFSTicket
verifyTicket merchantId merchantOperatingCity bapConfig vehicleCategory encryptedQrData platformType = do
  -- TODO: Add support for multiple integratedBPPConfigs, when we have multiple integratedBPPConfigs integrating verifyTicket API
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory) platformType
  onStatusReq <- Flow.verifyTicket merchantId merchantOperatingCity integratedBPPConfig bapConfig encryptedQrData
  processOnStatus onStatusReq >>= \case
    DOnStatus.TicketVerificationSync ticket -> return ticket
    _ -> throwError $ InvalidRequest "Unsupported OnStatus Response."
  where
    processOnStatus onStatusReq = do
      let verificationOnStatusReq = DOnStatus.TicketVerification onStatusReq
      (merchant', booking') <- DOnStatus.validateRequest verificationOnStatusReq
      void $ CallExternalBPP.status merchantId merchantOperatingCity bapConfig booking' -- Doing one status call to sync the Ticket Status & Expiry Before Verifying
      DOnStatus.onStatus merchant' booking' verificationOnStatusReq
