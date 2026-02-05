module API.UI.CancelSearch (API, handler, cancelSearch') where

import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DPerson
import Environment
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import Servant hiding (throwError)
import SharedLogic.Cancel
import Storage.Beam.SystemConfigs ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.Person as QP
import Tools.Auth
import Tools.Constants
import Tools.Error

type API =
  "estimate"
    :> ( TokenAuth
           :> Capture "estimateId" (Id DEstimate.Estimate)
           :> "cancel"
           :> Post '[JSON] CancelAPIResponse
           :<|> TokenAuth
             :> Capture "estimateId" (Id DEstimate.Estimate)
             :> "cancelSearch"
             :> Post '[JSON] APISuccess.APISuccess
           :<|> TokenAuth
             :> Capture "estimateId" (Id DEstimate.Estimate)
             :> "rejectUpgrade"
             :> Post '[JSON] CancelAPIResponse
       )

handler :: FlowServer API
handler =
  cancelSearch
    :<|> cancelSearchV2
    :<|> rejectUpgrade

cancelSearch :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> FlowHandler CancelAPIResponse
cancelSearch (personId, merchantId) estimateId = withFlowHandlerAPI $ cancelSearch' (personId, merchantId) estimateId

cancelSearchV2 :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> FlowHandler APISuccess.APISuccess
cancelSearchV2 (personId, merchantId) estimateId = withFlowHandlerAPI $ do
  void $ cancelSearchUtil (personId, merchantId) estimateId
  return APISuccess.Success

rejectUpgrade :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> FlowHandler CancelAPIResponse
rejectUpgrade (personId, merchantId) estimateId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let personTags = fromMaybe [] person.customerNammaTags
  unless (rejectUpgradeTag `Yudhishthira.elemTagNameValue` personTags) $ do
    rejectUpgradeTagWithExpiry <- Yudhishthira.fetchNammaTagExpiry (cast person.merchantOperatingCityId) rejectUpgradeTag
    QP.updateCustomerTags (Just $ personTags <> [rejectUpgradeTagWithExpiry]) person.id
  cancelSearch' (personId, merchantId) estimateId

cancelSearch' :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> Flow CancelAPIResponse
cancelSearch' (personId, merchantId) estimateId = do
  expr <- try @_ @SearchCancelErrors $ cancelSearchUtil (personId, merchantId) estimateId
  case expr of
    Left (ActiveBookingPresent _e) -> return BookingAlreadyCreated
    Left (FailedToCancelSearch _e) -> return FailedToCancel
    Right _ -> return Success
