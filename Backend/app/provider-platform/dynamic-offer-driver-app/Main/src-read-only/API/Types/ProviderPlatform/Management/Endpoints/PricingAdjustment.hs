{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.PricingAdjustment where

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
import qualified Lib.Types.SpecialLocation
import Servant
import Servant.Client

data PricingAdjustment = PricingAdjustment
  { adjustmentId :: Kernel.Types.Id.Id Dashboard.Common.FareAdjustment,
    vehicleServiceTiers :: [Dashboard.Common.ServiceTierType],
    areas :: Kernel.Prelude.Maybe [Lib.Types.SpecialLocation.Area],
    mode :: PricingAdjustmentMode,
    status :: PricingAdjustmentStatus,
    baseFareScalePct :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    perKmRateScalePct :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    perMinRateScalePct :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    congestionScalePct :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    rolloutPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    validFrom :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    reason :: Kernel.Prelude.Text,
    createdBy :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingAdjustmentArmStat = PricingAdjustmentArmStat
  { serviceTier :: Dashboard.Common.ServiceTierType,
    arm :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    estimates :: Kernel.Prelude.Int,
    avgMultiplier :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    avgMaxFare :: Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingAdjustmentListRes = PricingAdjustmentListRes {adjustments :: [PricingAdjustment]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingAdjustmentMode
  = EXPERIMENT
  | SPIKE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingAdjustmentPreviewReq = PricingAdjustmentPreviewReq
  { vehicleServiceTier :: Dashboard.Common.ServiceTierType,
    baseFareScalePct :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    perKmRateScalePct :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    perMinRateScalePct :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    congestionScalePct :: Kernel.Prelude.Maybe Kernel.Prelude.Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PricingAdjustmentPreviewReq where
  hideSecrets = Kernel.Prelude.identity

data PricingAdjustmentPreviewRes = PricingAdjustmentPreviewRes {before :: PricingAdjustmentSnapshot, after :: PricingAdjustmentSnapshot}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingAdjustmentReq = PricingAdjustmentReq
  { vehicleServiceTiers :: [Dashboard.Common.ServiceTierType],
    areas :: Kernel.Prelude.Maybe [Lib.Types.SpecialLocation.Area],
    mode :: PricingAdjustmentMode,
    baseFareScalePct :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    perKmRateScalePct :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    perMinRateScalePct :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    congestionScalePct :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    rolloutPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    validFrom :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    reason :: Kernel.Prelude.Text,
    createdBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PricingAdjustmentReq where
  hideSecrets = Kernel.Prelude.identity

data PricingAdjustmentRes = PricingAdjustmentRes {adjustmentId :: Kernel.Types.Id.Id Dashboard.Common.FareAdjustment, status :: PricingAdjustmentStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingAdjustmentResultsRes = PricingAdjustmentResultsRes {windowFrom :: Kernel.Prelude.UTCTime, windowTill :: Kernel.Prelude.UTCTime, rows :: [PricingAdjustmentArmStat]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingAdjustmentSnapshot = PricingAdjustmentSnapshot
  { baseFare :: Kernel.Types.Common.HighPrecMoney,
    firstPerKmRate :: Kernel.Types.Common.HighPrecMoney,
    firstPerMinRate :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    congestionMultiplier :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingAdjustmentStatus
  = DRAFT
  | ACTIVE
  | ENDED
  | EXPIRED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingAdjustmentStatusReq = PricingAdjustmentStatusReq {status :: PricingAdjustmentStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PricingAdjustmentStatusReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("pricingAdjustment" :> (GetPricingAdjustmentList :<|> PostPricingAdjustmentCreate :<|> PostPricingAdjustmentUpdate :<|> PostPricingAdjustmentStatus :<|> PostPricingAdjustmentPreview :<|> GetPricingAdjustmentResults))

type GetPricingAdjustmentList = ("list" :> Get '[JSON] PricingAdjustmentListRes)

type PostPricingAdjustmentCreate = ("create" :> ReqBody '[JSON] PricingAdjustmentReq :> Post '[JSON] PricingAdjustmentRes)

type PostPricingAdjustmentUpdate =
  ( Capture "adjustmentId" (Kernel.Types.Id.Id Dashboard.Common.FareAdjustment) :> "update" :> ReqBody '[JSON] PricingAdjustmentReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostPricingAdjustmentStatus =
  ( Capture "adjustmentId" (Kernel.Types.Id.Id Dashboard.Common.FareAdjustment) :> "status" :> ReqBody '[JSON] PricingAdjustmentStatusReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostPricingAdjustmentPreview = ("preview" :> ReqBody '[JSON] PricingAdjustmentPreviewReq :> Post '[JSON] PricingAdjustmentPreviewRes)

type GetPricingAdjustmentResults = (Capture "adjustmentId" (Kernel.Types.Id.Id Dashboard.Common.FareAdjustment) :> "results" :> Get '[JSON] PricingAdjustmentResultsRes)

data PricingAdjustmentAPIs = PricingAdjustmentAPIs
  { getPricingAdjustmentList :: EulerHS.Types.EulerClient PricingAdjustmentListRes,
    postPricingAdjustmentCreate :: PricingAdjustmentReq -> EulerHS.Types.EulerClient PricingAdjustmentRes,
    postPricingAdjustmentUpdate :: Kernel.Types.Id.Id Dashboard.Common.FareAdjustment -> PricingAdjustmentReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPricingAdjustmentStatus :: Kernel.Types.Id.Id Dashboard.Common.FareAdjustment -> PricingAdjustmentStatusReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPricingAdjustmentPreview :: PricingAdjustmentPreviewReq -> EulerHS.Types.EulerClient PricingAdjustmentPreviewRes,
    getPricingAdjustmentResults :: Kernel.Types.Id.Id Dashboard.Common.FareAdjustment -> EulerHS.Types.EulerClient PricingAdjustmentResultsRes
  }

mkPricingAdjustmentAPIs :: (Client EulerHS.Types.EulerClient API -> PricingAdjustmentAPIs)
mkPricingAdjustmentAPIs pricingAdjustmentClient = (PricingAdjustmentAPIs {..})
  where
    getPricingAdjustmentList :<|> postPricingAdjustmentCreate :<|> postPricingAdjustmentUpdate :<|> postPricingAdjustmentStatus :<|> postPricingAdjustmentPreview :<|> getPricingAdjustmentResults = pricingAdjustmentClient

data PricingAdjustmentUserActionType
  = GET_PRICING_ADJUSTMENT_LIST
  | POST_PRICING_ADJUSTMENT_CREATE
  | POST_PRICING_ADJUSTMENT_UPDATE
  | POST_PRICING_ADJUSTMENT_STATUS
  | POST_PRICING_ADJUSTMENT_PREVIEW
  | GET_PRICING_ADJUSTMENT_RESULTS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''PricingAdjustmentUserActionType])
