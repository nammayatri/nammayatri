{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Lib.Yudhishthira.Types.KaalChakra where

import Data.Aeson
import Data.Singletons.TH
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Lib.Scheduler
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

data EmptyData = EmptyData deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data Chakra
  = Daily
  | Weekly
  | Monthly
  | Quarterly
  deriving (Eq, Ord, Show, Read, FromDhall, Generic, ToJSON, FromJSON, ToParamSchema, ToSchema)

$(mkHttpInstancesForEnum ''Chakra)
$(mkBeamInstancesForEnum ''Chakra)
genSingletons [''Chakra]
showSingInstance ''Chakra

instance JobProcessor Chakra where
  restoreAnyJobInfo :: Sing (e :: Chakra) -> Text -> Maybe (AnyJobInfo Chakra)
  restoreAnyJobInfo SDaily jobData = AnyJobInfo <$> restoreJobInfo SDaily jobData
  restoreAnyJobInfo SWeekly jobData = AnyJobInfo <$> restoreJobInfo SWeekly jobData
  restoreAnyJobInfo SMonthly jobData = AnyJobInfo <$> restoreJobInfo SMonthly jobData
  restoreAnyJobInfo SQuarterly jobData = AnyJobInfo <$> restoreJobInfo SQuarterly jobData

instance JobInfoProcessor 'Daily

instance JobInfoProcessor 'Weekly

instance JobInfoProcessor 'Monthly

instance JobInfoProcessor 'Quarterly

type instance JobContent 'Daily = EmptyData

type instance JobContent 'Weekly = EmptyData

type instance JobContent 'Monthly = EmptyData

type instance JobContent 'Quarterly = EmptyData

newtype QLimit = QLimit {getQLimit :: Int}

newtype QOffset = QOffset {getQOffset :: Int}
