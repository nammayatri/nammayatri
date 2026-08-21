module Domain.Action.UI.TicketVerify
  ( postMultimodalTicketVerify,
  )
where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as DP
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import qualified Storage.Queries.Person as QP
import Tools.Auth ()
import Tools.Error

-- | Peer-BAP short id anna-checker proxies ticket-verify requests to. TODO: promote
-- to a config field on `AppBackendBapInternal` once ops finalizes multi-BAP wiring.
peerBapMerchantShortId :: Text
peerBapMerchantShortId = "NAMMA_YATRI"

postMultimodalTicketVerify ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Value ->
    Flow Value
  )
postMultimodalTicketVerify (mbPersonId, _merchantId, _merchantOpCityId) city body = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  unless (person.role `elem` [DP.BUS_CHECKER, DP.BUS_DISPATCHER]) $ throwError AccessDenied
  bapInternal <- asks (.appBackendBapInternal)
  CallBAPInternal.postMultimodalTicketVerify bapInternal.apiKey bapInternal.url peerBapMerchantShortId city body
