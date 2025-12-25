{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Domain.Types.FRFSQuoteCategorySpec where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH as UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

data FRFSCategoryTag
  = ADULT_PRICE
  | CHILD_PRICE
  | SENIOR_CITIZEN_PRICE
  | STUDENT_PRICE
  | FEMALE_PRICE
  | MALE_PRICE
  | ADULT_QUANTITY
  | CHILD_QUANTITY
  | SENIOR_CITIZEN_QUANTITY
  | STUDENT_QUANTITY
  | FEMALE_QUANTITY
  | MALE_QUANTITY
  | TOTAL_ADULT_PRICE
  | TOTAL_CHILD_PRICE
  | TOTAL_SENIOR_CITIZEN_PRICE
  | TOTAL_STUDENT_PRICE
  | TOTAL_FEMALE_PRICE
  | TOTAL_MALE_PRICE
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(UtilsTH.mkBeamInstancesForEnumAndList ''FRFSCategoryTag)

data OfferedValue
  = FixedAmount HighPrecMoney
  | Percentage Double
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(UtilsTH.mkBeamInstancesForEnumAndList ''OfferedValue)
