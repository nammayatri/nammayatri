module Domain.Action.Beckn.FRFSSeller.IGM
  ( handleIssue,
    handleIssueStatus,
  )
where

import qualified API.UI.Issue as IssueCasts
import qualified Beckn.ACL.FRFSSeller.OnSearch as OnSearchACL
import qualified BecknV2.FRFS.Enums as SpecEnums
import qualified BecknV2.OnDemand.Enums as BecknSpec
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Domain.Types.FRFSTicketBooking as DBooking
import qualified Domain.Types.Merchant as DM
import Environment (Flow)
import qualified IGM.Enums as IGMEnums
import qualified IGM.Types as IGMSpec
import qualified IssueManagement.Beckn.ACL.Issue as IssueACL
import qualified IssueManagement.Beckn.ACL.IssueStatus as IssueStatusACL
import qualified IssueManagement.Common as IGMCommon
import qualified IssueManagement.Domain.Action.Beckn.Issue as DIssue
import qualified IssueManagement.Domain.Action.Beckn.IssueStatus as DIssueStatus
import IssueManagement.Domain.Types.Issue.IGMConfig (IGMConfig)
import qualified IssueManagement.Domain.Types.Issue.IGMIssue as DIGM
import qualified IssueManagement.SharedLogic.CallAPI as IGMCallAPI
import qualified IssueManagement.Storage.Queries.Issue.IGMConfig as QIGMConfig
import qualified IssueManagement.Storage.Queries.Issue.IGMIssue as QIGM
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common
import qualified SharedLogic.FRFSSeller.Common as Common
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.FRFSTicketBooking as QBooking
import Tools.Error

data SellerContext = SellerContext
  { merchant :: DM.Merchant,
    igmMerchant :: IGMCommon.Merchant,
    igmCity :: IGMCommon.MerchantOperatingCity,
    self :: OnSearchACL.SellerIdentity
  }

data IssueFailure
  = OrderNotFound Text
  | UnsupportedTransition Text
  | Unprocessable Text

failureMessage :: IssueFailure -> Text
failureMessage = \case
  OrderNotFound orderId -> "No order found for order id " <> orderId
  UnsupportedTransition reason -> reason
  Unprocessable reason -> reason

sellerContext :: Text -> Flow SellerContext
sellerContext operator = do
  merchant <-
    CQM.findByShortId (Common.operatorMerchantShortId operator)
      >>= fromMaybeM (MerchantDoesNotExist operator)
  becknConfig <-
    QBC.findByMerchantIdDomainAndVehicle merchant.id (show SpecEnums.FRFS) BecknSpec.METRO
      >>= fromMaybeM (BecknConfigNotFound $ "merchantId:" <> merchant.id.getId <> " domain:FRFS vehicle:METRO")
  igmMerchant <-
    IssueCasts.castMerchantById (cast merchant.id)
      >>= fromMaybeM (MerchantDoesNotExist operator)
  let igmMerchantForSelf = igmMerchant{IGMCommon.subscriberId = ShortId becknConfig.subscriberId}
  moCity <-
    CQMOC.findByMerchantIdAndCity merchant.id merchant.defaultCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ merchant.id.getId <> "-" <> show merchant.defaultCity)
  igmCity <-
    IssueCasts.castMOCityById (cast moCity.id)
      >>= fromMaybeM (MerchantOperatingCityNotFound moCity.id.getId)
  pure
    SellerContext
      { merchant,
        igmMerchant = igmMerchantForSelf,
        igmCity,
        self =
          OnSearchACL.SellerIdentity
            { subscriberId = becknConfig.subscriberId,
              subscriberUrl = showBaseUrl becknConfig.subscriberUrl
            }
      }

handleIssue :: Text -> IGMSpec.IssueReq -> Flow ()
handleIssue operator req = do
  let ctx = req.context
  transactionId <- ctx.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId missing on issue context")
  messageId <- ctx.contextMessageId & fromMaybeM (InvalidRequest "MessageId missing on issue context")
  bapId <- ctx.contextBapId & fromMaybeM (InvalidRequest "BapId missing on issue context")
  bapUriText <- ctx.contextBapUri & fromMaybeM (InvalidRequest "BapUri missing on issue context")
  bapUri <- parseBaseUrl bapUriText
  dIssue <- IssueACL.buildIssueReq req
  seller <- sellerContext operator
  recordIssue seller dIssue >>= \case
    Left failure -> logWarning $ "FRFS seller issue rejected: " <> failureMessage failure
    Right issueRes -> do
      onIssueReq <- IssueACL.buildOnIssueReq transactionId messageId bapId bapUriText issueRes
      void $ IGMCallAPI.callOnIssue onIssueReq bapUri seller.igmMerchant

recordIssue :: SellerContext -> DIssue.DIssue -> Flow (Either IssueFailure DIssue.IssueRes)
recordIssue seller dIssue = runExceptT $ do
  issueStatus <- lift $ DIssue.mapStatusAndTypeToStatus dIssue.issueStatusText dIssue.issueTypeText
  issueType <- lift $ DIssue.mapType dIssue.issueTypeText
  booking <-
    lift (QBooking.findByBppOrderId (Just dIssue.bookingId))
      >>= maybe (throwE (OrderNotFound dIssue.bookingId)) pure
  unless (Common.isSellerRider booking.riderId) $
    throwE (OrderNotFound dIssue.bookingId)
  igmConfig <-
    lift (QIGMConfig.findByMerchantId (cast seller.merchant.id))
      >>= maybe (throwE (Unprocessable $ "No IGM config for merchant " <> seller.merchant.id.getId <> " - seed igm_config for FRFS_SELLER_*")) pure
  now <- lift getCurrentTime
  case issueStatus of
    DIGM.OPEN -> lift $ openIssue seller booking igmConfig dIssue issueType now
    DIGM.ESCALATED -> escalateIssue seller igmConfig dIssue now
    DIGM.CLOSED -> throwE (UnsupportedTransition "A seller does not close an issue on the buyer's behalf")
    DIGM.RESOLVED -> throwE (UnsupportedTransition "Issue already resolved")

openIssue :: SellerContext -> DBooking.FRFSTicketBooking -> IGMConfig -> DIssue.DIssue -> DIGM.IssueType -> UTCTime -> Flow DIssue.IssueRes
openIssue seller booking igmConfig dIssue issueType now = do
  QIGM.findByPrimaryKey (Id (Common.sellerIssueId dIssue.issueId)) >>= \case
    Just existing -> do
      logInfo $ "FRFS seller issue: " <> dIssue.issueId <> " is already raised; re-answering its stored state"
      pure (mkIssueRes seller igmConfig dIssue existing.issueStatus (UTCTimeRFC3339 existing.createdAt) (UTCTimeRFC3339 existing.updatedAt))
    Nothing -> raise
  where
    raise = do
      transactionId <- generateGUID
      let becknSubscriberId = seller.igmMerchant.subscriberId.getShortId
      QIGM.create
        DIGM.IGMIssue
          { DIGM.id = Id (Common.sellerIssueId dIssue.issueId),
            DIGM.createdAt = convertRFC3339ToUTC dIssue.createdAt,
            DIGM.updatedAt = convertRFC3339ToUTC dIssue.createdAt,
            DIGM.customerEmail = dIssue.customerEmail,
            DIGM.customerName = dIssue.customerName,
            DIGM.customerPhone = dIssue.customerPhone,
            DIGM.riderId = Nothing,
            DIGM.respondingMerchantId = Just becknSubscriberId,
            DIGM.respondentEntityType = Nothing,
            DIGM.transactionId = transactionId,
            DIGM.merchantOperatingCityId = Just (cast booking.merchantOperatingCityId),
            DIGM.bookingId = booking.id.getId,
            DIGM.issueRaisedByMerchant = Just dIssue.bapId,
            DIGM.issueStatus = DIGM.OPEN,
            DIGM.domain = IGMEnums.PUBLIC_TRANSPORT,
            DIGM.issueType = issueType,
            DIGM.respondentAction = Nothing,
            DIGM.respondentName = Nothing,
            DIGM.respondentEmail = Nothing,
            DIGM.respondentPhone = Nothing,
            DIGM.merchantId = Just (cast seller.merchant.id)
          }
      pure (mkIssueRes seller igmConfig dIssue DIGM.OPEN dIssue.createdAt (UTCTimeRFC3339 now))

escalateIssue :: SellerContext -> IGMConfig -> DIssue.DIssue -> UTCTime -> ExceptT IssueFailure Flow DIssue.IssueRes
escalateIssue seller igmConfig dIssue now = do
  existing <-
    lift (QIGM.findByPrimaryKey (Id (Common.sellerIssueId dIssue.issueId)))
      >>= maybe (throwE (UnsupportedTransition $ "Cannot escalate unknown issue " <> dIssue.issueId <> "; raise it as an ISSUE first")) pure
  lift $
    QIGM.updateByPrimaryKey
      existing
        { DIGM.issueStatus = DIGM.ESCALATED,
          DIGM.issueType = DIGM.GRIEVANCE,
          DIGM.updatedAt = now
        }
  pure (mkIssueRes seller igmConfig dIssue DIGM.ESCALATED (UTCTimeRFC3339 existing.createdAt) (UTCTimeRFC3339 now))

mkIssueRes :: SellerContext -> IGMConfig -> DIssue.DIssue -> DIGM.Status -> UTCTimeRFC3339 -> UTCTimeRFC3339 -> DIssue.IssueRes
mkIssueRes seller igmConfig dIssue issueStatus createdAt updatedAt =
  DIssue.IssueRes
    { issueId = dIssue.issueId,
      respondentAction = show IGMEnums.PROCESSING,
      groName = igmConfig.groName,
      groPhone = igmConfig.groPhone,
      groEmail = igmConfig.groEmail,
      createdAt,
      updatedAt,
      merchant = seller.igmMerchant,
      merchantOperatingCity = seller.igmCity,
      issueStatus,
      bapId = dIssue.bapId,
      bppId = seller.self.subscriberId
    }

handleIssueStatus :: Text -> IGMSpec.IssueStatusReq -> Flow ()
handleIssueStatus operator req = do
  let ctx = req.issueStatusReqContext
  transactionId <- ctx.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId missing on issue_status context")
  messageId <- ctx.contextMessageId & fromMaybeM (InvalidRequest "MessageId missing on issue_status context")
  bapId <- ctx.contextBapId & fromMaybeM (InvalidRequest "BapId missing on issue_status context")
  bapUriText <- ctx.contextBapUri & fromMaybeM (InvalidRequest "BapUri missing on issue_status context")
  bapUri <- parseBaseUrl bapUriText
  dIssueStatus <- IssueStatusACL.buildIssueStatusReq req
  seller <- sellerContext operator
  igmConfig <-
    QIGMConfig.findByMerchantId (cast seller.merchant.id)
      >>= fromMaybeM (InternalError $ "No IGM config for merchant " <> seller.merchant.id.getId)
  issue <-
    QIGM.findByPrimaryKey (Id (Common.sellerIssueId dIssueStatus.issueId))
      >>= fromMaybeM (InvalidRequest $ "Issue not found: " <> dIssueStatus.issueId)
  -- Both identities: issueRaisedByMerchant is stored as the opening bap_id (see handleIssue
  -- below), so without it any BAP that learns an issue id gets the status at its own bap_uri.
  unless (issue.merchantId == Just (cast seller.merchant.id) && issue.issueRaisedByMerchant == Just bapId) $
    throwError (InvalidRequest $ "Issue not found: " <> dIssueStatus.issueId)
  issueStatusRes <-
    DIssueStatus.handler
      DIssueStatus.ValidatedDIssueStatus
        { issue,
          merchant = seller.igmMerchant,
          igmConfig,
          merchantOperatingCity = seller.igmCity,
          bapId
        }
  let echoed = issueStatusRes{DIssueStatus.issueId = Id dIssueStatus.issueId}
  onIssueStatusReq <- IssueStatusACL.buildOnIssueStatusReq transactionId messageId bapId bapUriText echoed
  void $ IGMCallAPI.callOnIssueStatus onIssueStatusReq bapUri seller.igmMerchant
