{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module IssueManagement.Storage.Beam.Issue.IssueOption where

import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified IGM.Enums as Spec
import qualified IssueManagement.Common as Common
import IssueManagement.Tools.UtilsTH hiding (label)

data IssueOptionT f = IssueOptionT
  { id :: B.C f Text,
    issueCategoryId :: B.C f (Maybe Text),
    merchantOperatingCityId :: B.C f Text,
    option :: B.C f Text,
    priority :: B.C f Int,
    issueMessageId :: B.C f (Maybe Text),
    label :: B.C f (Maybe Text),
    merchantId :: B.C f Text,
    isActive :: B.C f Bool,
    restrictedVariants :: B.C f [Common.VehicleVariant],
    restrictedRideStatuses :: B.C f [Common.RideStatus],
    showOnlyWhenUserBlocked :: B.C f Bool,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime,
    igmSubCategory :: B.C f (Maybe Spec.IssueSubCategory),
    mandatoryUploads :: B.C f (Maybe [Common.MandatoryUploads])
  }
  deriving (Generic, B.Beamable)

instance B.Table IssueOptionT where
  data PrimaryKey IssueOptionT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type IssueOption = IssueOptionT Identity

$(enableKVPG ''IssueOptionT ['id] [['issueCategoryId]])

$(mkTableInstancesGenericSchema ''IssueOptionT "issue_option")
