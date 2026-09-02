{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.CancellationConsequence where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data CancellationConsequenceListItem = CancellationConsequenceListItem {rowId :: Kernel.Types.Id.Id Dashboard.Common.CancellationConsequenceMatrix, row :: CancellationConsequenceRowAPI}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CancellationConsequenceListRes = CancellationConsequenceListRes {rows :: [CancellationConsequenceListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CancellationConsequenceRowAPI = CancellationConsequenceRowAPI
  { faultVerdict :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    faultRule :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cancelledBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tripCategory :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleServiceTier :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    area :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentInstrument :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerDeduction :: Kernel.Prelude.Maybe DeductionAPIEntity,
    customerCommissionAndTax :: Kernel.Prelude.Maybe CommissionAndTaxAPI,
    driverDeduction :: Kernel.Prelude.Maybe DeductionAPIEntity,
    collectionMode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    waiveOffAllowed :: Kernel.Prelude.Bool,
    maxWaiveOffsPerPeriod :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    waiveOffPeriodDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    blacklistDriverForRiderSeconds :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    countsTowardDriverCancellationRate :: Kernel.Prelude.Bool,
    countsTowardCustomerCancellationStats :: Kernel.Prelude.Bool,
    exemptDashboardBookings :: Kernel.Prelude.Bool,
    driverNotificationKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerNotificationKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    active :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChargeRateAPI
  = FixedRateAPIEntity Kernel.Types.Common.HighPrecMoney
  | PercentageRateAPIEntity Kernel.Types.Common.HighPrecMoney
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CoinDeductionAPI = CoinDeductionAPI {coins :: Kernel.Prelude.Int, expirySeconds :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CommissionAndTaxAPI = CommissionAndTaxAPI
  { taxPercentage :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    commission :: Kernel.Prelude.Maybe ChargeRateAPI,
    amountsInclusiveOfTax :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateCancellationConsequenceReq = CreateCancellationConsequenceReq {row :: CancellationConsequenceRowAPI}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateCancellationConsequenceReq where
  hideSecrets = Kernel.Prelude.identity

data DeductionAPIEntity
  = CoinDeductionAPIEntity CoinDeductionAPI
  | MoneyDeductionAPIEntity MoneyDeductionAPI
  | CoinAdditionAPIEntity CoinDeductionAPI
  | MoneyAdditionAPIEntity MoneyDeductionAPI
  | RideCreditDeductionAPIEntity MoneyDeductionAPI
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FaultRuleRegistryEntryAPI = FaultRuleRegistryEntryAPI {name :: Kernel.Prelude.Text, description :: Kernel.Prelude.Text, active :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FaultRuleRegistryListRes = FaultRuleRegistryListRes {entries :: [FaultRuleRegistryEntryAPI]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FixedMoneyAPI = FixedMoneyAPI {amount :: Kernel.Types.Common.HighPrecMoney, overdueAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MoneyDeductionAPI
  = FixedMoneyAPIEntity FixedMoneyAPI
  | PercentageMoneyAPIEntity PercentageMoneyAPI
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PercentageMoneyAPI = PercentageMoneyAPI
  { percentage :: Kernel.Types.Common.HighPrecMoney,
    minAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    maxAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateCancellationConsequenceReq = UpdateCancellationConsequenceReq {rowId :: Kernel.Types.Id.Id Dashboard.Common.CancellationConsequenceMatrix, row :: CancellationConsequenceRowAPI}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateCancellationConsequenceReq where
  hideSecrets = Kernel.Prelude.identity

data UpsertFaultRuleRegistryReq = UpsertFaultRuleRegistryReq {name :: Kernel.Prelude.Text, description :: Kernel.Prelude.Text, active :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpsertFaultRuleRegistryReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("cancellationConsequence" :> (GetCancellationConsequenceList :<|> PostCancellationConsequenceCreate :<|> PostCancellationConsequenceUpdate :<|> GetCancellationConsequenceRegistryList :<|> PostCancellationConsequenceRegistryUpsert))

type GetCancellationConsequenceList = ("list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> Get '[JSON] CancellationConsequenceListRes)

type PostCancellationConsequenceCreate = ("create" :> ReqBody '[JSON] CreateCancellationConsequenceReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostCancellationConsequenceUpdate = ("update" :> ReqBody '[JSON] UpdateCancellationConsequenceReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetCancellationConsequenceRegistryList = ("registry" :> "list" :> Get '[JSON] FaultRuleRegistryListRes)

type PostCancellationConsequenceRegistryUpsert = ("registry" :> "upsert" :> ReqBody '[JSON] UpsertFaultRuleRegistryReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data CancellationConsequenceAPIs = CancellationConsequenceAPIs
  { getCancellationConsequenceList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient CancellationConsequenceListRes,
    postCancellationConsequenceCreate :: CreateCancellationConsequenceReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postCancellationConsequenceUpdate :: UpdateCancellationConsequenceReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getCancellationConsequenceRegistryList :: EulerHS.Types.EulerClient FaultRuleRegistryListRes,
    postCancellationConsequenceRegistryUpsert :: UpsertFaultRuleRegistryReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkCancellationConsequenceAPIs :: (Client EulerHS.Types.EulerClient API -> CancellationConsequenceAPIs)
mkCancellationConsequenceAPIs cancellationConsequenceClient = (CancellationConsequenceAPIs {..})
  where
    getCancellationConsequenceList :<|> postCancellationConsequenceCreate :<|> postCancellationConsequenceUpdate :<|> getCancellationConsequenceRegistryList :<|> postCancellationConsequenceRegistryUpsert = cancellationConsequenceClient

data CancellationConsequenceUserActionType
  = GET_CANCELLATION_CONSEQUENCE_LIST
  | POST_CANCELLATION_CONSEQUENCE_CREATE
  | POST_CANCELLATION_CONSEQUENCE_UPDATE
  | GET_CANCELLATION_CONSEQUENCE_REGISTRY_LIST
  | POST_CANCELLATION_CONSEQUENCE_REGISTRY_UPSERT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''CancellationConsequenceUserActionType])
