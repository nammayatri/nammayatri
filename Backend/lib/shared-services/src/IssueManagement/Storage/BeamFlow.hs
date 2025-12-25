module IssueManagement.Storage.BeamFlow where

import qualified IssueManagement.Storage.Beam.Issue.Comment as BeamC
import qualified IssueManagement.Storage.Beam.Issue.IGMConfig as BeamIGMC
import qualified IssueManagement.Storage.Beam.Issue.IGMIssue as BeamIGI
import qualified IssueManagement.Storage.Beam.Issue.IssueCategory as BeamIC
import qualified IssueManagement.Storage.Beam.Issue.IssueChat as BeamIC
import qualified IssueManagement.Storage.Beam.Issue.IssueConfig as BeamIC
import qualified IssueManagement.Storage.Beam.Issue.IssueMessage as BeamIM
import qualified IssueManagement.Storage.Beam.Issue.IssueOption as BeamIO
import qualified IssueManagement.Storage.Beam.Issue.IssueReport as BeamIR
import qualified IssueManagement.Storage.Beam.Issue.IssueTranslation as BeamIT
import qualified IssueManagement.Storage.Beam.MediaFile as BeamMF
import IssueManagement.Tools.UtilsTH
import Kernel.Utils.Common

type BeamFlow m r =
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    HasSchemaName BeamC.CommentT,
    HasSchemaName BeamIGI.IGMIssueT,
    HasSchemaName BeamIGMC.IGMConfigT,
    HasSchemaName BeamIC.IssueCategoryT,
    HasSchemaName BeamIC.IssueConfigT,
    HasSchemaName BeamIM.IssueMessageT,
    HasSchemaName BeamIO.IssueOptionT,
    HasSchemaName BeamIR.IssueReportT,
    HasSchemaName BeamIT.IssueTranslationT,
    HasSchemaName BeamIC.IssueChatT,
    HasSchemaName BeamMF.MediaFileT
  )
