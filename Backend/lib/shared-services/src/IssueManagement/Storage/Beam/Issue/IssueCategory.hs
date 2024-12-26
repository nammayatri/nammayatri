{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module IssueManagement.Storage.Beam.Issue.IssueCategory where

import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified IssueManagement.Common as Common
import qualified IssueManagement.Domain.Types.Issue.IssueCategory as IC
import IssueManagement.Tools.UtilsTH hiding (label)

data IssueCategoryT f = IssueCategoryT
  { id :: B.C f Text,
    category :: B.C f Text,
    merchantOperatingCityId :: B.C f Text,
    logoUrl :: B.C f Text,
    igmCategory :: B.C f (Maybe Text),
    priority :: B.C f Int,
    merchantId :: B.C f Text,
    categoryType :: B.C f IC.CategoryType,
    isRideRequired :: B.C f Bool,
    maxAllowedRideAge :: B.C f (Maybe Seconds),
    allowedRideStatuses :: B.C f (Maybe [Common.RideStatus]),
    label :: B.C f (Maybe Text),
    isActive :: B.C f Bool,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IssueCategoryT where
  data PrimaryKey IssueCategoryT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type IssueCategory = IssueCategoryT Identity

$(enableKVPG ''IssueCategoryT ['id] [['category]])

$(mkTableInstancesGenericSchema ''IssueCategoryT "issue_category")
