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
  )
where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.HideSecrets
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
    queryResults :: [Text],
    queryText :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

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

newtype RunKaalChakraJobReq = RunKaalChakraJobReq
  { chakra :: Chakra
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets RunKaalChakraJobReq where
  hideSecrets = identity
