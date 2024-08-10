{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Lib.Yudhishthira.Types.KaalChakra where

import Data.Singletons.TH
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Dhall (FromDhall)
import Lib.Scheduler
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
  | Quaterly
  deriving (Eq, Ord, Show, Read, FromDhall, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''Chakra)
genSingletons [''Chakra]
showSingInstance ''Chakra

instance JobProcessor Chakra where
  restoreAnyJobInfo :: Sing (e :: Chakra) -> Text -> Maybe (AnyJobInfo Chakra)
  restoreAnyJobInfo SDaily jobData = AnyJobInfo <$> restoreJobInfo SDaily jobData
  restoreAnyJobInfo SWeekly jobData = AnyJobInfo <$> restoreJobInfo SWeekly jobData
  restoreAnyJobInfo SMonthly jobData = AnyJobInfo <$> restoreJobInfo SMonthly jobData
  restoreAnyJobInfo SQuaterly jobData = AnyJobInfo <$> restoreJobInfo SQuaterly jobData

instance JobInfoProcessor 'Daily

instance JobInfoProcessor 'Weekly

instance JobInfoProcessor 'Monthly

instance JobInfoProcessor 'Quaterly

type instance JobContent 'Daily = EmptyData

type instance JobContent 'Weekly = EmptyData

type instance JobContent 'Monthly = EmptyData

type instance JobContent 'Quaterly = EmptyData

data ChakraQuery = ChakraQuery
  { query :: Text,
    queryResults :: [ChakraQueryResult]
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data ChakraQueryResultType = DOUBLE | BOOL | STRING deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

type ChakraQueryResult = (Text, ChakraQueryResultType)
