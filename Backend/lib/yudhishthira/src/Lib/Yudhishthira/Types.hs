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
    RunKaalChakraJobRes (..),
    RunKaalChakraJobResForUser (..),
    TagAPIEntity (..),
    UsersSet (..),
    QueryResult (..),
    QueryResultDefault (..),
    -- DynamicPricingResult (..),
  )
where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.HideSecrets
import Kernel.Types.Id
import Kernel.Types.TimeBound
import Kernel.Utils.TH
import Lib.Yudhishthira.Types.Application as Reexport
import Lib.Yudhishthira.Types.Common as Reexport
import Lib.Yudhishthira.Types.KaalChakra as Reexport
import Lib.Yudhishthira.Types.Manual as Reexport
import Lib.Yudhishthira.Types.Tag as Reexport

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
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnumAndList ''LogicDomain)
$(mkHttpInstancesForEnum ''LogicDomain)

data AppDynamicLogicReq = AppDynamicLogicReq
  { rules :: [Value],
    inputData :: [Value],
    shouldUpdateRule :: Maybe Bool,
    updatePassword :: Maybe Text,
    timeBounds :: Maybe TimeBound,
    domain :: LogicDomain
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data AppDynamicLogicResp = AppDynamicLogicResp
  { result :: Value,
    isRuleUpdated :: Bool,
    errors :: [String]
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data RunLogicResp = RunLogicResp
  { result :: Value,
    errors :: [String]
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets AppDynamicLogicReq where
  hideSecrets = identity

data RunKaalChakraJobReq = RunKaalChakraJobReq
  { chakra :: Chakra,
    updateUserTags :: Bool,
    parseQueryResults :: Bool,
    usersSet :: UsersSet,
    usersInBatch :: Int,
    maxBatches :: Int, -- we need to avoid endless loops in case of any query is wrong
    batchDelayInSec :: Int
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data UsersSet = SINGLE_USER (Id User) | LIST_USERS [Id User] | ALL_USERS
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data RunKaalChakraJobRes = RunKaalChakraJobRes
  { eventId :: Id Event,
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
