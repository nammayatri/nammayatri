{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module IssueManagement.Storage.BeamFlow where

import qualified IssueManagement.Storage.Beam.Issue.Comment as BeamC
import qualified IssueManagement.Storage.Beam.Issue.IssueCategory as BeamIC
import qualified IssueManagement.Storage.Beam.Issue.IssueConfig as BeamIC
import qualified IssueManagement.Storage.Beam.Issue.IssueMessage as BeamIM
import qualified IssueManagement.Storage.Beam.Issue.IssueOption as BeamIO
import qualified IssueManagement.Storage.Beam.Issue.IssueReport as BeamIR
import qualified IssueManagement.Storage.Beam.Issue.IssueTranslation as BeamIT
import qualified IssueManagement.Storage.Beam.MediaFile as BeamMF
import IssueManagement.Tools.UtilsTH

-- It's required for reusing Beam tables since we can define schema name for tables only on application level, not in lib
type BeamFlow m =
  ( MonadFlow m,
    HasSchemaName BeamC.CommentT,
    HasSchemaName BeamIC.IssueCategoryT,
    HasSchemaName BeamIC.IssueConfigT,
    HasSchemaName BeamIM.IssueMessageT,
    HasSchemaName BeamIO.IssueOptionT,
    HasSchemaName BeamIR.IssueReportT,
    HasSchemaName BeamIT.IssueTranslationT,
    HasSchemaName BeamMF.MediaFileT
  )
