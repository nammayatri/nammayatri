module Domain.Types.FRFSQuoteCategoryType where

import Kernel.Prelude

data FRFSQuoteCategoryType = ADULT | CHILD | SENIOR_CITIZEN | STUDENT | FEMALE | MALE | ADULT_SLEEPER | CHILD_SLEEPER deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)
