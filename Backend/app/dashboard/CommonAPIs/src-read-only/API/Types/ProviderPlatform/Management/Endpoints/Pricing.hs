{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Pricing where

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
import qualified Kernel.Types.TimeBound
import qualified Lib.Types.SpecialLocation
import Servant
import Servant.Client

data PricingEngineShare = PricingEngineShare {engine :: Kernel.Prelude.Text, estimates :: Kernel.Prelude.Int, avgMultiplier :: Kernel.Prelude.Maybe Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingEstimateExplainRes = PricingEstimateExplainRes
  { estimateId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    vehicleServiceTier :: Dashboard.Common.ServiceTierType,
    tripCategory :: Dashboard.Common.TripCategory,
    minFare :: Kernel.Types.Common.HighPrecMoney,
    maxFare :: Kernel.Types.Common.HighPrecMoney,
    engine :: Kernel.Prelude.Text,
    dpVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    congestionMultiplier :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    supplyDemandRatioFromLoc :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    supplyDemandRatioToLoc :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    fromLocGeohash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    smartTipSuggestion :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    smartTipReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    shadowSurgeMultiplier :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    shadowSurgeVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingGeohashStat = PricingGeohashStat {geohash :: Kernel.Prelude.Text, estimates :: Kernel.Prelude.Int, avgMultiplier :: Kernel.Prelude.Maybe Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingHealthRes = PricingHealthRes
  { windowHours :: Kernel.Prelude.Int,
    tiers :: [PricingTierHealth],
    engines :: [PricingEngineShare],
    topGeohashes :: [PricingGeohashStat],
    shadowComparison :: [PricingShadowComparison]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingShadowComparison = PricingShadowComparison
  { serviceTier :: Dashboard.Common.ServiceTierType,
    shadowVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    estimatesWithShadow :: Kernel.Prelude.Int,
    avgShadowMultiplier :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    avgAppliedMultiplier :: Kernel.Prelude.Maybe Kernel.Prelude.Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingSurgeConfig = PricingSurgeConfig
  { surgeConfigId :: Kernel.Types.Id.Id Dashboard.Common.SurgeConfig,
    vehicleServiceTier :: Dashboard.Common.ServiceTierType,
    timeBounds :: Kernel.Types.TimeBound.TimeBound,
    version :: Kernel.Prelude.Int,
    status :: PricingSurgeStatus,
    rows :: [PricingSurgeRow],
    minMultiplier :: Kernel.Types.Common.Centesimal,
    maxMultiplier :: Kernel.Types.Common.Centesimal,
    maxDeltaPerUpdate :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    applyOnExtraDistanceOnly :: Kernel.Prelude.Bool,
    excludedAreas :: Kernel.Prelude.Maybe [Lib.Types.SpecialLocation.Area],
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdBy :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingSurgeConfigListRes = PricingSurgeConfigListRes {configs :: [PricingSurgeConfig]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingSurgeConfigReq = PricingSurgeConfigReq
  { vehicleServiceTier :: Dashboard.Common.ServiceTierType,
    timeBounds :: Kernel.Types.TimeBound.TimeBound,
    rows :: [PricingSurgeRow],
    minMultiplier :: Kernel.Types.Common.Centesimal,
    maxMultiplier :: Kernel.Types.Common.Centesimal,
    maxDeltaPerUpdate :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    applyOnExtraDistanceOnly :: Kernel.Prelude.Bool,
    excludedAreas :: Kernel.Prelude.Maybe [Lib.Types.SpecialLocation.Area],
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PricingSurgeConfigReq where
  hideSecrets = Kernel.Prelude.identity

data PricingSurgeConfigRes = PricingSurgeConfigRes {surgeConfigId :: Kernel.Types.Id.Id Dashboard.Common.SurgeConfig, version :: Kernel.Prelude.Int, status :: PricingSurgeStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingSurgePreviewReq = PricingSurgePreviewReq
  { surgeConfigId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.SurgeConfig),
    rows :: Kernel.Prelude.Maybe [PricingSurgeRow],
    minMultiplier :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    maxMultiplier :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    signals :: PricingSurgeSignals
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PricingSurgePreviewReq where
  hideSecrets = Kernel.Prelude.identity

data PricingSurgePreviewRes = PricingSurgePreviewRes
  { matchedRowIndex :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    congestionMultiplier :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    congestionPerMinCharge :: Kernel.Prelude.Maybe Kernel.Prelude.Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingSurgeRow = PricingSurgeRow
  { qarMin :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    qarMax :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    supplyDemandRatioMin :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    supplyDemandRatioMax :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    distanceKmMin :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    distanceKmMax :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    congestionMultiplier :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    congestionPerMinCharge :: Kernel.Prelude.Maybe Kernel.Prelude.Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingSurgeSignals = PricingSurgeSignals {qar :: Kernel.Prelude.Maybe Kernel.Prelude.Double, supplyDemandRatio :: Kernel.Prelude.Maybe Kernel.Prelude.Double, distanceKm :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingSurgeStatus
  = DRAFT
  | SHADOW
  | ACTIVE
  | ARCHIVED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PricingSurgeStatusReq = PricingSurgeStatusReq {status :: PricingSurgeStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PricingSurgeStatusReq where
  hideSecrets = Kernel.Prelude.identity

data PricingTierHealth = PricingTierHealth
  { serviceTier :: Dashboard.Common.ServiceTierType,
    totalEstimates :: Kernel.Prelude.Int,
    decidedEstimates :: Kernel.Prelude.Int,
    surgedEstimates :: Kernel.Prelude.Int,
    avgMultiplier :: Kernel.Prelude.Maybe Kernel.Prelude.Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("pricing" :> (GetPricingSurgeList :<|> PostPricingSurgeCreate :<|> PostPricingSurgeUpdate :<|> PostPricingSurgeStatus :<|> PostPricingSurgePreview :<|> GetPricingObservabilityEstimate :<|> GetPricingObservabilityHealth))

type GetPricingSurgeList = ("surge" :> "list" :> QueryParam "serviceTier" Dashboard.Common.ServiceTierType :> Get '[JSON] PricingSurgeConfigListRes)

type PostPricingSurgeCreate = ("surge" :> "create" :> ReqBody '[JSON] PricingSurgeConfigReq :> Post '[JSON] PricingSurgeConfigRes)

type PostPricingSurgeUpdate =
  ( "surge" :> Capture "surgeConfigId" (Kernel.Types.Id.Id Dashboard.Common.SurgeConfig) :> "update" :> ReqBody '[JSON] PricingSurgeConfigReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostPricingSurgeStatus =
  ( "surge" :> Capture "surgeConfigId" (Kernel.Types.Id.Id Dashboard.Common.SurgeConfig) :> "status" :> ReqBody '[JSON] PricingSurgeStatusReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostPricingSurgePreview = ("surge" :> "preview" :> ReqBody '[JSON] PricingSurgePreviewReq :> Post '[JSON] PricingSurgePreviewRes)

type GetPricingObservabilityEstimate = ("observability" :> "estimate" :> Capture "estimateId" Kernel.Prelude.Text :> Get '[JSON] PricingEstimateExplainRes)

type GetPricingObservabilityHealth = ("observability" :> "health" :> QueryParam "hours" Kernel.Prelude.Int :> Get '[JSON] PricingHealthRes)

data PricingAPIs = PricingAPIs
  { getPricingSurgeList :: Kernel.Prelude.Maybe Dashboard.Common.ServiceTierType -> EulerHS.Types.EulerClient PricingSurgeConfigListRes,
    postPricingSurgeCreate :: PricingSurgeConfigReq -> EulerHS.Types.EulerClient PricingSurgeConfigRes,
    postPricingSurgeUpdate :: Kernel.Types.Id.Id Dashboard.Common.SurgeConfig -> PricingSurgeConfigReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPricingSurgeStatus :: Kernel.Types.Id.Id Dashboard.Common.SurgeConfig -> PricingSurgeStatusReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postPricingSurgePreview :: PricingSurgePreviewReq -> EulerHS.Types.EulerClient PricingSurgePreviewRes,
    getPricingObservabilityEstimate :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient PricingEstimateExplainRes,
    getPricingObservabilityHealth :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient PricingHealthRes
  }

mkPricingAPIs :: (Client EulerHS.Types.EulerClient API -> PricingAPIs)
mkPricingAPIs pricingClient = (PricingAPIs {..})
  where
    getPricingSurgeList :<|> postPricingSurgeCreate :<|> postPricingSurgeUpdate :<|> postPricingSurgeStatus :<|> postPricingSurgePreview :<|> getPricingObservabilityEstimate :<|> getPricingObservabilityHealth = pricingClient

data PricingUserActionType
  = GET_PRICING_SURGE_LIST
  | POST_PRICING_SURGE_CREATE
  | POST_PRICING_SURGE_UPDATE
  | POST_PRICING_SURGE_STATUS
  | POST_PRICING_SURGE_PREVIEW
  | GET_PRICING_OBSERVABILITY_ESTIMATE
  | GET_PRICING_OBSERVABILITY_HEALTH
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''PricingUserActionType])
