module IssueManagement.SharedLogic.IGM where

import qualified IGM.Enums as Spec
import IssueManagement.Common
import qualified IssueManagement.Domain.Action.Beckn.IssueStatus as DBecknIssueStatus
import qualified IssueManagement.Domain.Types.Issue.IGMConfig as DIGMC
import qualified IssueManagement.Domain.Types.Issue.IGMIssue as DIGM
import Kernel.Prelude
import Kernel.Types.TimeRFC339

mkIssueStatusRes ::
  DIGM.IGMIssue ->
  DIGMC.IGMConfig ->
  Merchant ->
  MerchantOperatingCity ->
  Text ->
  Spec.RespondentActions ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  UTCTime ->
  DBecknIssueStatus.IssueStatusRes
mkIssueStatusRes igmIssue igmConfig merchant merchantOpCity bapId action resShortDesc resLongDesc resActionTriggered resRefundAmount now =
  DBecknIssueStatus.IssueStatusRes
    { issueId = igmIssue.id,
      issueStatus = igmIssue.issueStatus,
      respondentAction = show action,
      groName = igmConfig.groName,
      groPhone = igmConfig.groPhone,
      groEmail = igmConfig.groEmail,
      respondentName = fromMaybe igmConfig.groName igmConfig.respondentName,
      respondentPhone = fromMaybe igmConfig.groPhone igmConfig.respondentPhone,
      respondentEmail = fromMaybe igmConfig.groEmail igmConfig.respondentEmail,
      resolutionProviderName = fromMaybe igmConfig.groName igmConfig.resolutionProviderName,
      resolutionProviderPhone = fromMaybe igmConfig.groPhone igmConfig.resolutionProviderPhone,
      resolutionProviderEmail = fromMaybe igmConfig.groEmail igmConfig.resolutionProviderEmail,
      merchant = merchant,
      merchantOperatingCity = merchantOpCity,
      createdAt = UTCTimeRFC3339 igmIssue.createdAt,
      updatedAt = UTCTimeRFC3339 now,
      bapId = bapId,
      domain = igmIssue.domain,
      resolutionShortDesc = resShortDesc,
      resolutionLongDesc = resLongDesc,
      resolutionActionTriggered = resActionTriggered,
      resolutionRefundAmount = resRefundAmount,
      isValueAddNP = False
    }
