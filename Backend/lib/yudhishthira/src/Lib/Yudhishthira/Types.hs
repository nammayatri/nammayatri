module Lib.Yudhishthira.Types
  ( module Reexport,
    YudhishthiraDecideReq (..),
    YudhishthiraDecideResp (..),
    ChakraQueriesAPIEntity (..),
    Source (..),
    SourceData,
    CreateNammaTagRequest (..),
    LogicDomain (..),
    AppDynamicLogicReq (..),
    AppDynamicLogicResp (..),
    RunLogicResp (..),
    RunKaalChakraJobReq (..),
    KaalChakraAction (..),
    KaalChakraJobData (..),
    mkKaalChakraJobData,
    RunKaalChakraJobRes (..),
    RunKaalChakraJobResForUser (..),
    TagAPIEntity (..),
    UsersSet (..),
    QueryResult (..),
    QueryResultDefault (..),
    UpdateNammaTagRequest (..),
    GetLogicsResp (..),
    LogicRolloutObject (..),
    RolloutVersion (..),
    CreateTimeBoundRequest (..),
    LogicRolloutReq,
    -- DynamicPricingResult (..),
  )
where

import Control.Lens.Operators hiding ((.=))
import Data.Aeson
import Data.OpenApi as OpenApi hiding (description, name, tags, version)
import qualified Data.Text as T
import Domain.Types.ServiceTierType as DST
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.HideSecrets
import Kernel.Types.Id
import Kernel.Types.TimeBound
import Kernel.Utils.Common
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Lib.Scheduler.Types (AnyJob)
import Lib.Yudhishthira.Types.Application as Reexport
import Lib.Yudhishthira.Types.Common as Reexport
import Lib.Yudhishthira.Types.KaalChakra as Reexport
import Lib.Yudhishthira.Types.Manual as Reexport
import Lib.Yudhishthira.Types.Tag as Reexport
import qualified Text.Show (show)

data Source
  = Application ApplicationEvent
  | KaalChakra Chakra
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

type SourceData = Value -- json to be decoded in the respective tag

data CreateNammaTagRequest
  = ApplicationTag NammaTagApplication
  | KaalChakraTag NammaTagChakra
  | ManualTag NammaTagManual
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets CreateNammaTagRequest where
  hideSecrets = identity

data UpdateNammaTagRequest = UpdateNammaTagRequest
  { tagCategory :: Maybe Text,
    tagName :: Text,
    description :: Maybe Text,
    tagPossibleValues :: Maybe TagValues,
    tagChakra :: Maybe Chakra,
    tagValidity :: Maybe Hours,
    tagStage :: Maybe ApplicationEvent,
    tagRule :: Maybe TagRule
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets UpdateNammaTagRequest where
  hideSecrets = identity

data ChakraQueriesAPIEntity = ChakraQueriesAPIEntity
  { chakra :: Chakra,
    queryName :: Text,
    queryResults :: [QueryResult],
    queryText :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data QueryResult = QueryResult
  { resultName :: Text,
    resultDefault :: QueryResultDefault
  }
  deriving (Generic, Show, Eq, Ord, Read, ToJSON, FromJSON, ToSchema)

data QueryResultDefault = BOOL Bool | INT Int | DOUBLE Double | TEXT Text
  deriving (Generic, Show, Eq, Ord, Read, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''QueryResult)

instance HideSecrets ChakraQueriesAPIEntity where
  hideSecrets = identity

data YudhishthiraDecideReq = YudhishthiraDecideReq
  { source :: Source,
    sourceData :: SourceData
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

newtype YudhishthiraDecideResp = YudhishthiraDecideResp
  { tags :: [NammaTagResponse]
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data LogicDomain
  = POOLING
  | FARE_POLICY
  | DYNAMIC_PRICING DST.ServiceTierType
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

generateLogicDomainShowInstances :: [String]
generateLogicDomainShowInstances =
  [show POOLING]
    ++ [show FARE_POLICY]
    ++ [show (DYNAMIC_PRICING vehicleType) | vehicleType <- vehicleTypes]
  where
    vehicleTypes = [COMFY, ECO, PREMIUM, SUV, AUTO_RICKSHAW, HATCHBACK, SEDAN, TAXI, TAXI_PLUS, PREMIUM_SEDAN, BLACK, BLACK_XL, BIKE, AMBULANCE_TAXI, AMBULANCE_TAXI_OXY, AMBULANCE_AC, AMBULANCE_AC_OXY, AMBULANCE_VENTILATOR, SUV_PLUS, DELIVERY_BIKE]

instance ToParamSchema LogicDomain where
  toParamSchema _ =
    mempty
      & title ?~ "LogicDomain"
      & type_ ?~ OpenApiString
      & enum_
        ?~ map (String . T.pack) generateLogicDomainShowInstances

instance Show LogicDomain where
  show POOLING = "POOLING"
  show FARE_POLICY = "FARE-POLICY"
  show (DYNAMIC_PRICING vehicleType) =
    "DYNAMIC-PRICING_" ++ show vehicleType

instance Read LogicDomain where
  readsPrec _ s =
    let (prefx, rest) = break (== '_') s
     in case prefx of
          "POOLING" ->
            [(POOLING, drop 1 rest)]
          "FARE-POLICY" ->
            [(FARE_POLICY, drop 1 rest)]
          "DYNAMIC-PRICING" ->
            let (vehicleTypeStr, rest1) = break (== '_') (drop 1 rest)
             in case readMaybe vehicleTypeStr of
                  Just vehicleType ->
                    [(DYNAMIC_PRICING vehicleType, rest1)]
                  _ -> []
          _ -> []

$(mkBeamInstancesForEnumAndList ''LogicDomain)
$(mkHttpInstancesForEnum ''LogicDomain)

data AppDynamicLogicReq = AppDynamicLogicReq
  { rules :: [Value],
    inputData :: [Value],
    shouldUpdateRule :: Maybe Bool,
    updatePassword :: Maybe Text,
    domain :: LogicDomain
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data AppDynamicLogicResp = AppDynamicLogicResp
  { result :: Value,
    isRuleUpdated :: Bool,
    domain :: LogicDomain,
    version :: Maybe Int,
    errors :: [String]
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data GetLogicsResp = GetLogicsResp
  { domain :: LogicDomain,
    version :: Int,
    logics :: [Value]
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data CreateTimeBoundRequest = CreateTimeBoundRequest
  { timeBoundDomain :: LogicDomain,
    name :: Text,
    timeBounds :: TimeBound
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets CreateTimeBoundRequest where
  hideSecrets = identity

data RunLogicResp = RunLogicResp
  { result :: Value,
    errors :: [String]
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

type LogicRolloutReq = [LogicRolloutObject]

instance HideSecrets LogicRolloutReq where
  hideSecrets = identity

data LogicRolloutObject = LogicRolloutObject
  { domain :: LogicDomain,
    timeBounds :: Text,
    rollout :: [RolloutVersion]
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets LogicRolloutObject where
  hideSecrets = identity

data RolloutVersion = RolloutVersion
  { version :: Int,
    rolloutPercentage :: Int
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets RolloutVersion where
  hideSecrets = identity

instance HideSecrets AppDynamicLogicReq where
  hideSecrets = identity

data RunKaalChakraJobReq = RunKaalChakraJobReq
  { chakra :: Chakra,
    action :: KaalChakraAction,
    updateUserTags :: Bool,
    parseQueryResults :: Bool,
    usersSet :: UsersSet,
    usersInBatch :: Int,
    maxBatches :: Int, -- we need to avoid endless loops in case of any query is wrong
    batchDelayInSec :: Int,
    completeOldJob :: Maybe (Id AnyJob)
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data KaalChakraJobData = KaalChakraJobData
  { updateUserTags :: Bool,
    parseQueryResults :: Bool,
    usersInBatch :: Int,
    maxBatches :: Int,
    batchDelayInSec :: Int
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

mkKaalChakraJobData :: RunKaalChakraJobReq -> KaalChakraJobData
mkKaalChakraJobData RunKaalChakraJobReq {..} = KaalChakraJobData {..}

data KaalChakraAction = RUN | SCHEDULE UTCTime
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data UsersSet = SINGLE_USER (Id User) | LIST_USERS [Id User] | ALL_USERS
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data RunKaalChakraJobRes = RunKaalChakraJobRes
  { eventId :: Maybe (Id Event),
    tags :: Maybe [TagAPIEntity],
    users :: Maybe [RunKaalChakraJobResForUser]
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data TagAPIEntity = TagAPIEntity
  { name :: Text,
    possibleValues :: TagValues,
    rule :: TagRule,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data RunKaalChakraJobResForUser = RunKaalChakraJobResForUser
  { userId :: Id User,
    userDataValue :: Value, -- final result with default values
    userOldTags :: Maybe [Text], -- tagName#TAG_VALUE format
    userUpdatedTags :: Maybe [Text] -- tagName#TAG_VALUE format
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets RunKaalChakraJobReq where
  hideSecrets = identity

instance HideSecrets RunKaalChakraJobRes where
  hideSecrets = identity
