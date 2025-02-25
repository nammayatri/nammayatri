{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Lib.Yudhishthira.Types.KaalChakra where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.Common
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Lib.Yudhishthira.Types.Common

data NammaTagChakra = NammaTagChakra
  { tagCategory :: Text,
    tagName :: Text,
    description :: Maybe Text,
    tagPossibleValues :: TagValues,
    tagChakra :: Chakra,
    tagValidity :: Maybe Hours,
    tagRule :: TagRule,
    actionEngine :: Maybe Value
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data Chakra
  = Daily
  | Weekly
  | Monthly
  | Quarterly
  deriving (Eq, Ord, Show, Read, FromDhall, Generic, ToJSON, FromJSON, ToParamSchema, ToSchema)

$(mkHttpInstancesForEnum ''Chakra)
$(mkBeamInstancesForEnum ''Chakra)

instance CH.ClickhouseValue Chakra

newtype QLimit = QLimit {getQLimit :: Int}

newtype QOffset = QOffset {getQOffset :: Int}
