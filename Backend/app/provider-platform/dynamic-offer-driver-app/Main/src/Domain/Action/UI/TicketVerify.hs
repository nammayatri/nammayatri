module Domain.Action.UI.TicketVerify
  ( postMultimodalTicketVerify,
  )
where

import qualified API.Types.UI.TicketVerify
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MultimodalTicketVerify
import qualified Domain.Types.Person as DP
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Person as QP
import Tools.Auth ()
import Tools.Error

postMultimodalTicketVerify ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Beckn.Context.City ->
    API.Types.UI.TicketVerify.MultimodalTicketVerifyReq ->
    Flow Domain.Types.MultimodalTicketVerify.MultimodalTicketVerifyResp
  )
postMultimodalTicketVerify (mbPersonId, _merchantId, _merchantOpCityId) city body = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  -- Any operator may verify; they self-register through /auth and need no depot_manager row.
  unless (person.role == DP.PT_PROVIDER) $ throwError AccessDenied
  transporterConfig <-
    getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) Nothing
      >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  peerBapMerchantShortId <-
    transporterConfig.peerBapMerchantShortId
      & fromMaybeM (InvalidRequest $ "peerBapMerchantShortId not configured for city " <> person.merchantOperatingCityId.getId)
  bapInternal <- asks (.appBackendBapInternal)
  CallBAPInternal.postMultimodalTicketVerify bapInternal.apiKey bapInternal.url peerBapMerchantShortId city body
