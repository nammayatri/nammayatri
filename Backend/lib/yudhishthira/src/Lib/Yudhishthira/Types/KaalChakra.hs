{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Lib.Yudhishthira.Types.KaalChakra where

import Data.Singletons.TH
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Dhall (FromDhall)
import Lib.Yudhishthira.Types.Common

data NammaTagChakra = NammaTagChakra
  { tagCategory :: Text,
    tagName :: Text,
    description :: Maybe Text,
    tagPossibleValues :: TagValues,
    tagChakra :: Chakra,
    tagValidity :: Maybe Hours,
    tagRule :: TagRule
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data EmptyData = EmptyData deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data Chakra
  = Daily
  | Weekly
  | Monthly
  | Quarterly
  deriving (Eq, Ord, Show, Read, FromDhall, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''Chakra)
genSingletons [''Chakra]
showSingInstance ''Chakra

newtype QLimit = QLimit {getQLimit :: Int}

newtype QOffset = QOffset {getQOffset :: Int}
