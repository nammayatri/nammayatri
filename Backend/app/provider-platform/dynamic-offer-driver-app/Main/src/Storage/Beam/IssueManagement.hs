{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.IssueManagement where

import qualified Data.Text as T
import qualified IssueManagement.Storage.Beam.Issue.Comment as BeamC
import qualified IssueManagement.Storage.Beam.Issue.IssueCategory as BeamIC
import qualified IssueManagement.Storage.Beam.Issue.IssueConfig as BeamIS
import qualified IssueManagement.Storage.Beam.Issue.IssueMessage as BeamIM
import qualified IssueManagement.Storage.Beam.Issue.IssueOption as BeamIO
import qualified IssueManagement.Storage.Beam.Issue.IssueReport as BeamIR
import qualified IssueManagement.Storage.Beam.Issue.IssueTranslation as BeamIT
import qualified IssueManagement.Storage.Beam.MediaFile as BeamM
import Tools.Beam.UtilsTH (HasSchemaName (..), currentSchemaName)

instance HasSchemaName BeamIC.IssueCategoryT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamC.CommentT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamIS.IssueConfigT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamIM.IssueMessageT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamIO.IssueOptionT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamIR.IssueReportT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamIT.IssueTranslationT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamM.MediaFileT where
  schemaName _ = T.pack currentSchemaName
