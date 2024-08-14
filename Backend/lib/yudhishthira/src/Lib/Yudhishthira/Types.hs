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
  )
where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.HideSecrets
import Lib.Yudhishthira.Types.Application as Reexport
import Lib.Yudhishthira.Types.Common as Reexport
import Lib.Yudhishthira.Types.KaalChakra as Reexport
import Lib.Yudhishthira.Types.Tag as Reexport

data Source
  = Application ApplicationEvent
  | KaalChakra Chakra
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

type SourceData = Text -- json to be decoded in the respective tag

data CreateNammaTagRequest
  = ApplicationTag NammaTagApplication
  | KaalChakraTag NammaTagChakra
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets CreateNammaTagRequest where
  hideSecrets = identity

data ChakraQueriesAPIEntity = ChakraQueriesAPIEntity
  { chakra :: Chakra,
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
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''LogicDomain)

data AppDynamicLogicReq = AppDynamicLogicReq
  { rules :: [Value],
    inputData :: [Value],
    domain :: LogicDomain
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data AppDynamicLogicResp = AppDynamicLogicResp
  { result :: Value,
    errors :: [String]
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets AppDynamicLogicReq where
  hideSecrets = identity
