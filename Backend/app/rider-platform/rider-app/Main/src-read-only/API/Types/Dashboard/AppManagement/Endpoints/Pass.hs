{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.Pass where

import qualified "this" API.Types.UI.Pass
import qualified BecknV2.FRFS.Enums
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
import qualified "this" Domain.Types.Pass
import qualified "this" Domain.Types.PassCategory
import qualified "this" Domain.Types.PassType
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.PurchasedPass
import qualified "this" Domain.Types.PurchasedPassPayment
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified "shared-services" IssueManagement.Common.UI.Issue
import qualified "shared-services" IssueManagement.Domain.Types.MediaFile
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import qualified "payment" Lib.Payment.Domain.Action
import qualified "payment" Lib.Payment.Domain.Types.PaymentOrder
import Servant
import Servant.Client

data FixedSavingAPIEntity = FixedSavingAPIEntity {applicableValue :: Kernel.Types.Common.HighPrecMoney, currencyType :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency, enabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OverrideBenefitAPIEntity = OverrideBenefitAPIEntity
  { fixedSaving :: Kernel.Prelude.Maybe FixedSavingAPIEntity,
    maxTicketQuantityPerOverride :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maximumTripCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    percentageSaving :: Kernel.Prelude.Maybe PercentageSavingAPIEntity,
    unlimitedTripCount :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassCatalogItem = PassCatalogItem
  { amount :: Kernel.Types.Common.HighPrecMoney,
    applicableVehicleServiceTiers :: [BecknV2.FRFS.Enums.ServiceTierType],
    autoApply :: Kernel.Prelude.Bool,
    benefit :: Kernel.Prelude.Maybe Domain.Types.Pass.Benefit,
    benefitDescription :: Kernel.Prelude.Text,
    code :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documentsRequired :: [Domain.Types.Pass.PassDocumentType],
    enable :: Kernel.Prelude.Bool,
    formVerificationConfig :: Kernel.Prelude.Maybe Data.Aeson.Value,
    id :: Kernel.Types.Id.Id Domain.Types.Pass.Pass,
    maxFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    maxSwitchCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxValidDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxValidTrips :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Int,
    passCategoryName :: Kernel.Prelude.Text,
    passTypeId :: Kernel.Types.Id.Id Domain.Types.PassType.PassType,
    passTypeTitle :: Kernel.Prelude.Text,
    pricingTiers :: Kernel.Prelude.Maybe Data.Aeson.Value,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    verificationValidity :: Kernel.Types.Common.Seconds
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassCategoryCreateReq = PassCategoryCreateReq {description :: Kernel.Prelude.Text, name :: Kernel.Prelude.Text, order :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PassCategoryCreateReq where
  hideSecrets = Kernel.Prelude.identity

data PassCategoryCreateResp = PassCategoryCreateResp {passCategoryId :: Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassCategoryItem = PassCategoryItem
  { description :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory,
    name :: Kernel.Prelude.Text,
    order :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    passTypeCount :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassCategoryUpdateReq = PassCategoryUpdateReq {description :: Kernel.Prelude.Maybe Kernel.Prelude.Text, name :: Kernel.Prelude.Maybe Kernel.Prelude.Text, order :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PassCategoryUpdateReq where
  hideSecrets = Kernel.Prelude.identity

data PassCreateReq = PassCreateReq
  { amount :: Kernel.Types.Common.HighPrecMoney,
    applicableVehicleServiceTiers :: [BecknV2.FRFS.Enums.ServiceTierType],
    autoApply :: Kernel.Prelude.Bool,
    benefit :: Kernel.Prelude.Maybe Domain.Types.Pass.Benefit,
    benefitDescription :: Kernel.Prelude.Text,
    code :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documentsRequired :: [Domain.Types.Pass.PassDocumentType],
    enable :: Kernel.Prelude.Bool,
    formVerificationConfig :: Kernel.Prelude.Maybe Data.Aeson.Value,
    maxFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    maxSwitchCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxValidDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxValidTrips :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Int,
    passTypeId :: Kernel.Types.Id.Id Domain.Types.PassType.PassType,
    pricingTiers :: Kernel.Prelude.Maybe Data.Aeson.Value,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    verificationValidity :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PassCreateReq where
  hideSecrets = Kernel.Prelude.identity

data PassCreateResp = PassCreateResp {passId :: Kernel.Types.Id.Id Domain.Types.Pass.Pass}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassOverrideConfig = PassOverrideConfig
  { benefit :: Kernel.Prelude.Maybe OverrideBenefitAPIEntity,
    benefitConfigError :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    code :: Kernel.Prelude.Text,
    frfsCancelLimit :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    frfsPriceOverrideApplicable :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    minDaysToSuggestRenewal :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minTripsAllowingOverlap :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    passId :: Kernel.Types.Id.Id Domain.Types.Pass.Pass,
    timeOverlappingFrfsBookingsLimit :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassOverrideUpdateReq = PassOverrideUpdateReq
  { benefit :: Kernel.Prelude.Maybe OverrideBenefitAPIEntity,
    frfsCancelLimit :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    frfsPriceOverrideApplicable :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    minDaysToSuggestRenewal :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minTripsAllowingOverlap :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    timeOverlappingFrfsBookingsLimit :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PassOverrideUpdateReq where
  hideSecrets = Kernel.Prelude.identity

data PassTripAdjustReq = PassTripAdjustReq
  { operation :: TripAdjustOperation,
    purchasedPassPaymentId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment),
    value :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PassTripAdjustReq where
  hideSecrets = Kernel.Prelude.identity

data PassTripAdjustResp = PassTripAdjustResp
  { previousTrips :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    purchasedPassPaymentId :: Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment,
    remainingTrips :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassTypeCreateReq = PassTypeCreateReq
  { catchline :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    maxPhotoChangeLimit :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Int,
    passCategoryId :: Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory,
    passEnum :: Kernel.Prelude.Maybe Domain.Types.PassType.PassEnum,
    photoReUploadTimeLimit :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    title :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PassTypeCreateReq where
  hideSecrets = Kernel.Prelude.identity

data PassTypeCreateResp = PassTypeCreateResp {passTypeId :: Kernel.Types.Id.Id Domain.Types.PassType.PassType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassTypeItem = PassTypeItem
  { catchline :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.PassType.PassType,
    maxPhotoChangeLimit :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Int,
    passCategoryId :: Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory,
    passCategoryName :: Kernel.Prelude.Text,
    passCount :: Kernel.Prelude.Int,
    passEnum :: Kernel.Prelude.Maybe Domain.Types.PassType.PassEnum,
    photoReUploadTimeLimit :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    title :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassTypeUpdateReq = PassTypeUpdateReq
  { catchline :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    maxPhotoChangeLimit :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    passCategoryId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory),
    passEnum :: Kernel.Prelude.Maybe Domain.Types.PassType.PassEnum,
    photoReUploadTimeLimit :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    title :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PassTypeUpdateReq where
  hideSecrets = Kernel.Prelude.identity

data PassUpdateReq = PassUpdateReq
  { amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    applicableVehicleServiceTiers :: Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType],
    autoApply :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    benefit :: Kernel.Prelude.Maybe Domain.Types.Pass.Benefit,
    benefitDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    code :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    documentsRequired :: Kernel.Prelude.Maybe [Domain.Types.Pass.PassDocumentType],
    enable :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    formVerificationConfig :: Kernel.Prelude.Maybe Data.Aeson.Value,
    maxFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    maxSwitchCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxValidDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    maxValidTrips :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    passTypeId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassType.PassType),
    pricingTiers :: Kernel.Prelude.Maybe Data.Aeson.Value,
    vehicleType :: Kernel.Prelude.Maybe BecknV2.FRFS.Enums.VehicleCategory,
    verificationValidity :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PassUpdateReq where
  hideSecrets = Kernel.Prelude.identity

data PercentageSavingAPIEntity = PercentageSavingAPIEntity {applicableValue :: Kernel.Types.Common.HighPrecMoney, enabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PurchasedPassSelectReq = PurchasedPassSelectReq {startDay :: Kernel.Prelude.Maybe Data.Time.Day, profilePicture :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PurchasedPassSelectReq where
  hideSecrets = Kernel.Prelude.identity

data TripAdjustOperation
  = IncrementBy
  | DecrementBy
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("pass" :> (GetPassCustomerAvailablePasses :<|> GetPassCustomerPurchasedPasses :<|> GetPassCustomerTransactions :<|> PostPassCustomerActivateToday :<|> PostPassCustomerPassSelectHelper :<|> GetPassCustomerPaymentStatusHelper :<|> PostPassCustomerPassResetDeviceSwitchCount :<|> PostPassCustomerPassUpdateProfilePicture :<|> GetPassCustomerPassPhoto :<|> PostPassCustomerPassRestore :<|> ListPassCatalog :<|> CreatePass :<|> UpdatePass :<|> DeletePass :<|> ListPassCategories :<|> CreatePassCategory :<|> UpdatePassCategory :<|> ListPassTypes :<|> CreatePassType :<|> UpdatePassType :<|> GetPassOverrideConfig :<|> UpdatePassOverrideConfig :<|> PostPassTripsAdjust))

type GetPassCustomerAvailablePasses =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "availablePasses"
      :> QueryParam
           "language"
           Kernel.External.Types.Language
      :> Get '[JSON] [API.Types.UI.Pass.PassInfoAPIEntity]
  )

type GetPassCustomerPurchasedPasses =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "purchasedPasses"
      :> QueryParam
           "language"
           Kernel.External.Types.Language
      :> QueryParam "status" Domain.Types.PurchasedPass.StatusType
      :> Get
           '[JSON]
           [API.Types.UI.Pass.PurchasedPassAPIEntity]
  )

type GetPassCustomerTransactions =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "transactions" :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "status" Kernel.Prelude.Text
      :> Get
           '[JSON]
           [API.Types.UI.Pass.PurchasedPassTransactionAPIEntity]
  )

type PostPassCustomerActivateToday =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "activateToday"
      :> Capture
           "passNumber"
           Kernel.Prelude.Int
      :> QueryParam
           "purchasedPassPaymentId"
           (Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment)
      :> QueryParam
           "startDay"
           Data.Time.Day
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostPassCustomerPassSelect =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "pass"
      :> Capture
           "passId"
           (Kernel.Types.Id.Id Domain.Types.Pass.Pass)
      :> "select"
      :> ReqBody '[JSON] PurchasedPassSelectReq
      :> Post '[JSON] API.Types.UI.Pass.PassSelectionAPIEntity
  )

type PostPassCustomerPassSelectHelper =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "pass"
      :> Capture
           "passId"
           (Kernel.Types.Id.Id Domain.Types.Pass.Pass)
      :> "select"
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           PurchasedPassSelectReq
      :> Post
           '[JSON]
           API.Types.UI.Pass.PassSelectionAPIEntity
  )

type GetPassCustomerPaymentStatus =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "payment"
      :> Capture
           "orderId"
           (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder)
      :> "status"
      :> Get '[JSON] Lib.Payment.Domain.Action.PaymentStatusResp
  )

type GetPassCustomerPaymentStatusHelper =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "payment"
      :> Capture
           "orderId"
           (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder)
      :> "status"
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> Get
           '[JSON]
           Lib.Payment.Domain.Action.PaymentStatusResp
  )

type PostPassCustomerPassResetDeviceSwitchCount =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "pass"
      :> Capture
           "passId"
           (Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass)
      :> "resetDeviceSwitchCount"
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostPassCustomerPassUpdateProfilePicture =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "pass"
      :> Capture
           "purchasedPassId"
           (Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass)
      :> "updateProfilePicture"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           IssueManagement.Common.UI.Issue.IssueMediaUploadReq
      :> Post
           '[JSON]
           IssueManagement.Common.UI.Issue.IssueMediaUploadRes
  )

type GetPassCustomerPassPhoto =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "pass" :> "photo"
      :> Capture
           "mediaId"
           (Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile)
      :> Get '[JSON] Kernel.Prelude.Text
  )

type PostPassCustomerPassRestore = ("customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "pass" :> "restore" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type ListPassCatalog =
  ( "catalog" :> "list" :> QueryParam "enable" Kernel.Prelude.Bool :> QueryParam "passTypeId" (Kernel.Types.Id.Id Domain.Types.PassType.PassType)
      :> Get
           '[JSON]
           [PassCatalogItem]
  )

type CreatePass = ("catalog" :> "create" :> ReqBody '[JSON] PassCreateReq :> Post '[JSON] PassCreateResp)

type UpdatePass = ("catalog" :> Capture "passId" (Kernel.Types.Id.Id Domain.Types.Pass.Pass) :> "update" :> ReqBody '[JSON] PassUpdateReq :> Put '[JSON] Kernel.Types.APISuccess.APISuccess)

type DeletePass = ("catalog" :> Capture "passId" (Kernel.Types.Id.Id Domain.Types.Pass.Pass) :> "delete" :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess)

type ListPassCategories = ("catalog" :> "passCategory" :> "list" :> Get '[JSON] [PassCategoryItem])

type CreatePassCategory = ("catalog" :> "passCategory" :> "create" :> ReqBody '[JSON] PassCategoryCreateReq :> Post '[JSON] PassCategoryCreateResp)

type UpdatePassCategory =
  ( "catalog" :> "passCategory" :> Capture "passCategoryId" (Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory) :> "update"
      :> ReqBody
           '[JSON]
           PassCategoryUpdateReq
      :> Put '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type ListPassTypes = ("catalog" :> "passType" :> "list" :> QueryParam "passCategoryId" (Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory) :> Get '[JSON] [PassTypeItem])

type CreatePassType = ("catalog" :> "passType" :> "create" :> ReqBody '[JSON] PassTypeCreateReq :> Post '[JSON] PassTypeCreateResp)

type UpdatePassType =
  ( "catalog" :> "passType" :> Capture "passTypeId" (Kernel.Types.Id.Id Domain.Types.PassType.PassType) :> "update" :> ReqBody '[JSON] PassTypeUpdateReq
      :> Put
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetPassOverrideConfig = ("catalog" :> Capture "passId" (Kernel.Types.Id.Id Domain.Types.Pass.Pass) :> "override" :> Get '[JSON] PassOverrideConfig)

type UpdatePassOverrideConfig =
  ( "catalog" :> Capture "passId" (Kernel.Types.Id.Id Domain.Types.Pass.Pass) :> "override" :> "update" :> ReqBody '[JSON] PassOverrideUpdateReq
      :> Put
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostPassTripsAdjust =
  ( "customer" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "pass"
      :> Capture
           "purchasedPassId"
           (Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass)
      :> "trips"
      :> "adjust"
      :> ReqBody '[JSON] PassTripAdjustReq
      :> Post '[JSON] PassTripAdjustResp
  )

data PassAPIs = PassAPIs
  { getPassCustomerAvailablePasses :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> EulerHS.Types.EulerClient [API.Types.UI.Pass.PassInfoAPIEntity],
    getPassCustomerPurchasedPasses :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Kernel.Prelude.Maybe Domain.Types.PurchasedPass.StatusType -> EulerHS.Types.EulerClient [API.Types.UI.Pass.PurchasedPassAPIEntity],
    getPassCustomerTransactions :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient [API.Types.UI.Pass.PurchasedPassTransactionAPIEntity],
    postPassCustomerActivateToday :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment) -> Kernel.Prelude.Maybe Data.Time.Day -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPassCustomerPassSelect :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Pass.Pass -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> PurchasedPassSelectReq -> EulerHS.Types.EulerClient API.Types.UI.Pass.PassSelectionAPIEntity,
    getPassCustomerPaymentStatus :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient Lib.Payment.Domain.Action.PaymentStatusResp,
    postPassCustomerPassResetDeviceSwitchCount :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPassCustomerPassUpdateProfilePicture ::
      Kernel.Types.Id.Id Domain.Types.Person.Person ->
      Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass ->
      ( Data.ByteString.Lazy.ByteString,
        IssueManagement.Common.UI.Issue.IssueMediaUploadReq
      ) ->
      EulerHS.Types.EulerClient IssueManagement.Common.UI.Issue.IssueMediaUploadRes,
    getPassCustomerPassPhoto :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile -> EulerHS.Types.EulerClient Kernel.Prelude.Text,
    postPassCustomerPassRestore :: Kernel.Types.Id.Id Domain.Types.Person.Person -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    listPassCatalog :: Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassType.PassType) -> EulerHS.Types.EulerClient [PassCatalogItem],
    createPass :: PassCreateReq -> EulerHS.Types.EulerClient PassCreateResp,
    updatePass :: Kernel.Types.Id.Id Domain.Types.Pass.Pass -> PassUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deletePass :: Kernel.Types.Id.Id Domain.Types.Pass.Pass -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    listPassCategories :: EulerHS.Types.EulerClient [PassCategoryItem],
    createPassCategory :: PassCategoryCreateReq -> EulerHS.Types.EulerClient PassCategoryCreateResp,
    updatePassCategory :: Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory -> PassCategoryUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    listPassTypes :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory) -> EulerHS.Types.EulerClient [PassTypeItem],
    createPassType :: PassTypeCreateReq -> EulerHS.Types.EulerClient PassTypeCreateResp,
    updatePassType :: Kernel.Types.Id.Id Domain.Types.PassType.PassType -> PassTypeUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getPassOverrideConfig :: Kernel.Types.Id.Id Domain.Types.Pass.Pass -> EulerHS.Types.EulerClient PassOverrideConfig,
    updatePassOverrideConfig :: Kernel.Types.Id.Id Domain.Types.Pass.Pass -> PassOverrideUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPassTripsAdjust :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> PassTripAdjustReq -> EulerHS.Types.EulerClient PassTripAdjustResp
  }

mkPassAPIs :: (Client EulerHS.Types.EulerClient API -> PassAPIs)
mkPassAPIs passClient = (PassAPIs {..})
  where
    getPassCustomerAvailablePasses :<|> getPassCustomerPurchasedPasses :<|> getPassCustomerTransactions :<|> postPassCustomerActivateToday :<|> postPassCustomerPassSelect :<|> getPassCustomerPaymentStatus :<|> postPassCustomerPassResetDeviceSwitchCount :<|> postPassCustomerPassUpdateProfilePicture :<|> getPassCustomerPassPhoto :<|> postPassCustomerPassRestore :<|> listPassCatalog :<|> createPass :<|> updatePass :<|> deletePass :<|> listPassCategories :<|> createPassCategory :<|> updatePassCategory :<|> listPassTypes :<|> createPassType :<|> updatePassType :<|> getPassOverrideConfig :<|> updatePassOverrideConfig :<|> postPassTripsAdjust = passClient

data PassUserActionType
  = GET_PASS_CUSTOMER_AVAILABLE_PASSES
  | GET_PASS_CUSTOMER_PURCHASED_PASSES
  | GET_PASS_CUSTOMER_TRANSACTIONS
  | POST_PASS_CUSTOMER_ACTIVATE_TODAY
  | POST_PASS_CUSTOMER_PASS_SELECT
  | GET_PASS_CUSTOMER_PAYMENT_STATUS
  | POST_PASS_CUSTOMER_PASS_RESET_DEVICE_SWITCH_COUNT
  | POST_PASS_CUSTOMER_PASS_UPDATE_PROFILE_PICTURE
  | GET_PASS_CUSTOMER_PASS_PHOTO
  | POST_PASS_CUSTOMER_PASS_RESTORE
  | LIST_PASS_CATALOG
  | CREATE_PASS
  | UPDATE_PASS
  | DELETE_PASS
  | LIST_PASS_CATEGORIES
  | CREATE_PASS_CATEGORY
  | UPDATE_PASS_CATEGORY
  | LIST_PASS_TYPES
  | CREATE_PASS_TYPE
  | UPDATE_PASS_TYPE
  | GET_PASS_OVERRIDE_CONFIG
  | UPDATE_PASS_OVERRIDE_CONFIG
  | POST_PASS_TRIPS_ADJUST
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''PassUserActionType])
