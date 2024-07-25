module Lib.Yudhishthira.Types
  ( module Reexport,
    YudhishthiraDecideReq (..),
    YudhishthiraDecideResp (..),
    Source (..),
    SourceData (..),
    NammaTag (..),
  )
where

import Kernel.Prelude
import Lib.Yudhishthira.Types.Application as Reexport
import Lib.Yudhishthira.Types.Common as Reexport
import Lib.Yudhishthira.Types.KaalChakra as Reexport
import Lib.Yudhishthira.Types.Tag as Reexport

data Source
  = Application ApplicationEvent
  | KaalChakra Chakra
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data SourceData
  = ApplicationD ApplicationData
  | KaalChakraD KaalChakraData
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data NammaTag
  = ApplicationTag NammaTagApplication
  | KaalChakraTag NammaTagChakra
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data YudhishthiraDecideReq = YudhishthiraDecideReq
  { source :: Source,
    sourceData :: SourceData
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

newtype YudhishthiraDecideResp = YudhishthiraDecideResp
  { tags :: [NammaTagResponse]
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)
