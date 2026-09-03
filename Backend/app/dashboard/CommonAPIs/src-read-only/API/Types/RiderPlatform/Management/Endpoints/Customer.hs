{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Customer where

import qualified Dashboard.Common
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.PaymentMode
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data ActionType
  = BLOCK
  | UNBLOCK
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ApplyCustomerOfferReq = ApplyCustomerOfferReq
  { mobileNumber :: Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    offerCode :: Kernel.Prelude.Text,
    validityHours :: Kernel.Prelude.Int,
    amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ApplyCustomerOfferReq where
  hideSecrets = Kernel.Prelude.identity

data BlockCustomerReq = BlockCustomerReq {blockedReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets BlockCustomerReq where
  hideSecrets = Kernel.Prelude.identity

data BulkApplyCustomerOfferReq = BulkApplyCustomerOfferReq
  { customers :: [BulkApplyOfferCustomer],
    offerCode :: Kernel.Prelude.Text,
    validityHours :: Kernel.Prelude.Int,
    amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets BulkApplyCustomerOfferReq where
  hideSecrets = Kernel.Prelude.identity

data BulkApplyCustomerOfferRes = BulkApplyCustomerOfferRes {mobileNumber :: Kernel.Prelude.Text, isSuccess :: Kernel.Prelude.Bool, errorReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BulkApplyOfferCustomer = BulkApplyOfferCustomer {mobileNumber :: Kernel.Prelude.Text, mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CancellationDueBreakup = CancellationDueBreakup {rideId :: Kernel.Types.Id.Id Dashboard.Common.Ride, dueAmount :: Kernel.Types.Common.PriceAPIEntity, dueStatus :: CancellationDuesPaymentStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CancellationDuesDetailsRes = CancellationDuesDetailsRes
  { cancellationDues :: Kernel.Types.Common.PriceAPIEntity,
    cancellationDuesPaid :: Kernel.Types.Common.HighPrecMoney,
    noOfTimesCancellationDuesPaid :: Kernel.Prelude.Int,
    waivedOffAmount :: Kernel.Types.Common.HighPrecMoney,
    noOfTimesWaiveOffUsed :: Kernel.Prelude.Int,
    duesBreakup :: Kernel.Prelude.Maybe [CancellationDueBreakup]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CancellationDuesPaymentStatus
  = PENDING
  | PAID
  | WAIVED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CustomerBlockTransactions = CustomerBlockTransactions
  { reasonCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blockReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blockTimeInHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    reportedAt :: Kernel.Prelude.UTCTime,
    blockLiftTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    blockedBy :: Kernel.Prelude.Text,
    requestorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    actionType :: Kernel.Prelude.Maybe ActionType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CustomerEnsureExistsReq = CustomerEnsureExistsReq {mobileNumber :: Kernel.Prelude.Text, mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CustomerEnsureExistsReq where
  hideSecrets = Kernel.Prelude.identity

data CustomerInfoRes = CustomerInfoRes
  { numberOfRides :: Kernel.Prelude.Int,
    falseSafetyAlarmCount :: Kernel.Prelude.Int,
    safetyCenterDisabledOnDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    totalSosCount :: Kernel.Prelude.Int,
    paymentMode :: Kernel.Prelude.Maybe Domain.Types.PaymentMode.PaymentMode,
    blockedReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blockedInfo :: [CustomerBlockTransactions],
    blockCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CustomerListItem = CustomerListItem
  { customerId :: Kernel.Types.Id.Id Dashboard.Common.Customer,
    firstName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    phoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    blocked :: Kernel.Prelude.Bool,
    paymentMode :: Kernel.Prelude.Maybe Domain.Types.PaymentMode.PaymentMode
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CustomerListRes = CustomerListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, customers :: [CustomerListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CustomerOfferEntity = CustomerOfferEntity
  { offerId :: Kernel.Prelude.Text,
    offerCode :: Kernel.Prelude.Text,
    offerTitle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    offerDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    autoApply :: Kernel.Prelude.Bool,
    isHidden :: Kernel.Prelude.Bool,
    amountSaved :: Kernel.Types.Common.HighPrecMoney,
    postOfferAmount :: Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CustomerOffersListReq = CustomerOffersListReq {mobileNumber :: Kernel.Prelude.Text, mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text, amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CustomerOffersListReq where
  hideSecrets = Kernel.Prelude.identity

newtype UpdatePaymentModeReq = UpdatePaymentModeReq {paymentMode :: Domain.Types.PaymentMode.PaymentMode}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdatePaymentModeReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateSafetyCenterBlockingReq = UpdateSafetyCenterBlockingReq {incrementCount :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, resetCount :: Kernel.Prelude.Maybe Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateSafetyCenterBlockingReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("customer" :> (GetCustomerList :<|> DeleteCustomerDelete :<|> PostCustomerBlockHelper :<|> PostCustomerUnblockHelper :<|> GetCustomerInfo :<|> GetCustomerCancellationDuesDetails :<|> PostCustomerUpdateSafetyCenterBlocking :<|> PostCustomerPersonNumbers :<|> PostCustomerPersonId :<|> PostCustomerUpdatePaymentMode :<|> PostCustomerOffersList :<|> PostCustomerApplyOffer :<|> PostCustomerEnsureExists :<|> PostCustomerBulkApplyOffer))

type GetCustomerList =
  ( "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "enabled" Kernel.Prelude.Bool
      :> QueryParam
           "blocked"
           Kernel.Prelude.Bool
      :> QueryParam "phone" Kernel.Prelude.Text
      :> QueryParam
           "countryCode"
           Kernel.Prelude.Text
      :> QueryParam
           "personId"
           (Kernel.Types.Id.Id Dashboard.Common.Customer)
      :> Get
           ('[JSON])
           CustomerListRes
  )

type DeleteCustomerDelete = (Capture "customerId" (Kernel.Types.Id.Id Dashboard.Common.Customer) :> "delete" :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostCustomerBlock = (Capture "customerId" (Kernel.Types.Id.Id Dashboard.Common.Customer) :> "block" :> ReqBody ('[JSON]) BlockCustomerReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostCustomerBlockHelper =
  ( Capture "customerId" (Kernel.Types.Id.Id Dashboard.Common.Customer) :> "block" :> Capture "dashboardUserName" Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           BlockCustomerReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type PostCustomerUnblock = (Capture "customerId" (Kernel.Types.Id.Id Dashboard.Common.Customer) :> "unblock" :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostCustomerUnblockHelper =
  ( Capture "customerId" (Kernel.Types.Id.Id Dashboard.Common.Customer) :> "unblock" :> Capture "dashboardUserName" Kernel.Prelude.Text
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type GetCustomerInfo = (Capture "customerId" (Kernel.Types.Id.Id Dashboard.Common.Customer) :> "info" :> Get ('[JSON]) CustomerInfoRes)

type GetCustomerCancellationDuesDetails = (Capture "customerId" (Kernel.Types.Id.Id Dashboard.Common.Customer) :> "getCancellationDuesDetails" :> Get ('[JSON]) CancellationDuesDetailsRes)

type PostCustomerUpdateSafetyCenterBlocking =
  ( Capture "customerId" (Kernel.Types.Id.Id Dashboard.Common.Customer) :> "updateSafetyCenterBlocking"
      :> ReqBody
           ('[JSON])
           UpdateSafetyCenterBlockingReq
      :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type PostCustomerPersonNumbers = ("personNumbers" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp Dashboard.Common.PersonIdsReq :> Post ('[JSON]) [Dashboard.Common.PersonRes])

type PostCustomerPersonId = ("personId" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp Dashboard.Common.PersonMobileNoReq :> Post ('[JSON]) [Dashboard.Common.PersonRes])

type PostCustomerUpdatePaymentMode =
  ( Capture "customerId" (Kernel.Types.Id.Id Dashboard.Common.Customer) :> "updatePaymentMode" :> ReqBody ('[JSON]) UpdatePaymentModeReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type PostCustomerOffersList = ("offersList" :> ReqBody ('[JSON]) CustomerOffersListReq :> Post ('[JSON]) [CustomerOfferEntity])

type PostCustomerApplyOffer = ("applyOffer" :> ReqBody ('[JSON]) ApplyCustomerOfferReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostCustomerEnsureExists = ("ensureExists" :> ReqBody ('[JSON]) CustomerEnsureExistsReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostCustomerBulkApplyOffer = ("bulkApplyOffer" :> ReqBody ('[JSON]) BulkApplyCustomerOfferReq :> Post ('[JSON]) [BulkApplyCustomerOfferRes])

data CustomerAPIs = CustomerAPIs
  { getCustomerList :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Customer) -> EulerHS.Types.EulerClient CustomerListRes),
    deleteCustomerDelete :: (Kernel.Types.Id.Id Dashboard.Common.Customer -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postCustomerBlock :: (Kernel.Types.Id.Id Dashboard.Common.Customer -> Kernel.Prelude.Text -> BlockCustomerReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postCustomerUnblock :: (Kernel.Types.Id.Id Dashboard.Common.Customer -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getCustomerInfo :: (Kernel.Types.Id.Id Dashboard.Common.Customer -> EulerHS.Types.EulerClient CustomerInfoRes),
    getCustomerCancellationDuesDetails :: (Kernel.Types.Id.Id Dashboard.Common.Customer -> EulerHS.Types.EulerClient CancellationDuesDetailsRes),
    postCustomerUpdateSafetyCenterBlocking :: (Kernel.Types.Id.Id Dashboard.Common.Customer -> UpdateSafetyCenterBlockingReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postCustomerPersonNumbers :: ((Data.ByteString.Lazy.ByteString, Dashboard.Common.PersonIdsReq) -> EulerHS.Types.EulerClient [Dashboard.Common.PersonRes]),
    postCustomerPersonId :: ((Data.ByteString.Lazy.ByteString, Dashboard.Common.PersonMobileNoReq) -> EulerHS.Types.EulerClient [Dashboard.Common.PersonRes]),
    postCustomerUpdatePaymentMode :: (Kernel.Types.Id.Id Dashboard.Common.Customer -> UpdatePaymentModeReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postCustomerOffersList :: (CustomerOffersListReq -> EulerHS.Types.EulerClient [CustomerOfferEntity]),
    postCustomerApplyOffer :: (ApplyCustomerOfferReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postCustomerEnsureExists :: (CustomerEnsureExistsReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postCustomerBulkApplyOffer :: (BulkApplyCustomerOfferReq -> EulerHS.Types.EulerClient [BulkApplyCustomerOfferRes])
  }

mkCustomerAPIs :: (Client EulerHS.Types.EulerClient API -> CustomerAPIs)
mkCustomerAPIs customerClient = (CustomerAPIs {..})
  where
    getCustomerList :<|> deleteCustomerDelete :<|> postCustomerBlock :<|> postCustomerUnblock :<|> getCustomerInfo :<|> getCustomerCancellationDuesDetails :<|> postCustomerUpdateSafetyCenterBlocking :<|> postCustomerPersonNumbers :<|> postCustomerPersonId :<|> postCustomerUpdatePaymentMode :<|> postCustomerOffersList :<|> postCustomerApplyOffer :<|> postCustomerEnsureExists :<|> postCustomerBulkApplyOffer = customerClient

data CustomerUserActionType
  = GET_CUSTOMER_LIST
  | DELETE_CUSTOMER_DELETE
  | POST_CUSTOMER_BLOCK
  | POST_CUSTOMER_UNBLOCK
  | GET_CUSTOMER_INFO
  | GET_CUSTOMER_CANCELLATION_DUES_DETAILS
  | POST_CUSTOMER_UPDATE_SAFETY_CENTER_BLOCKING
  | POST_CUSTOMER_PERSON_NUMBERS
  | POST_CUSTOMER_PERSON_ID
  | POST_CUSTOMER_UPDATE_PAYMENT_MODE
  | POST_CUSTOMER_OFFERS_LIST
  | POST_CUSTOMER_APPLY_OFFER
  | POST_CUSTOMER_ENSURE_EXISTS
  | POST_CUSTOMER_BULK_APPLY_OFFER
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''CustomerUserActionType)])
