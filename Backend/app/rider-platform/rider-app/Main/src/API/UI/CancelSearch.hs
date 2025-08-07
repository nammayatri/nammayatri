module API.UI.CancelSearch (API, handler, JLT.cancelSearch') where

import qualified Domain.Action.UI.Select as DSelect
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DPerson
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.JourneyLeg.Taxi ()
import qualified Lib.JourneyLeg.Taxi as JLT
import qualified Lib.JourneyModule.Base as JLT
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Person as QP
import Tools.Auth
import Tools.Constants
import Tools.Error

type API =
  "estimate"
    :> ( TokenAuth
           :> Capture "estimateId" (Id DEstimate.Estimate)
           :> "cancel"
           :> Post '[JSON] DSelect.CancelAPIResponse
           :<|> TokenAuth
             :> Capture "estimateId" (Id DEstimate.Estimate)
             :> "cancelSearch"
             :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> Capture "estimateId" (Id DEstimate.Estimate)
             :> "rejectUpgrade"
             :> Post '[JSON] DSelect.CancelAPIResponse
       )

handler :: FlowServer API
handler =
  cancelSearch
    :<|> cancelSearchV2
    :<|> rejectUpgrade

cancelSearch :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> FlowHandler DSelect.CancelAPIResponse
cancelSearch (personId, merchantId) estimateId = withFlowHandlerAPI $ do
  expr <- try @_ @SearchCancelErrors $ cancelSearchImpl (personId, merchantId) estimateId
  case expr of
    Left (ActiveBookingPresent _e) -> return DSelect.BookingAlreadyCreated
    Left (FailedToCancelSearch _e) -> return DSelect.FailedToCancel
    Right _ -> return DSelect.Success

cancelSearchV2 :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> FlowHandler APISuccess
cancelSearchV2 (personId, merchantId) estimateId = withFlowHandlerAPI $ cancelSearchImpl (personId, merchantId) estimateId

cancelSearchImpl :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> Flow APISuccess
cancelSearchImpl (_personId, _merchantId) estimateId = do
  estimate <- QEstimate.findById estimateId >>= fromMaybeM EstimateNotFound
  let searchId = estimate.requestId
  mbJourneyLeg <- QJourneyLeg.findByLegSearchId (Just searchId.getId)
  case mbJourneyLeg of
    Just journeyLeg -> do
      legInfo <- JLT.getLegInfo journeyLeg searchId.getId False >>= fromMaybeM (InvalidRequest $ "LegInfo not found for searchId: " <> searchId.getId)
      let cancellationReasonCode = SCR.CancellationReasonCode "SEARCH_CANCELLED_BY_RIDER"
      JLT.cancelLeg (journeyLeg.journeyId) legInfo cancellationReasonCode True True True (Just estimateId)
    Nothing -> do
      void $ JLT.cancelSearch' (_personId, _merchantId) estimateId
  return Success

rejectUpgrade :: (Id DPerson.Person, Id Merchant.Merchant) -> Id DEstimate.Estimate -> FlowHandler DSelect.CancelAPIResponse
rejectUpgrade (personId, merchantId) estimateId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let personTags = fromMaybe [] person.customerNammaTags
  unless (rejectUpgradeTag `Yudhishthira.elemTagNameValue` personTags) $ do
    rejectUpgradeTagWithExpiry <- Yudhishthira.fetchNammaTagExpiry rejectUpgradeTag
    QP.updateCustomerTags (Just $ personTags <> [rejectUpgradeTagWithExpiry]) person.id
  JLT.cancelSearchUtil (personId, merchantId) estimateId
