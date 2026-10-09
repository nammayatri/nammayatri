module IssueManagement.SharedLogic.IGM where

import qualified IGM.Enums as Spec
import IssueManagement.Common
import IssueManagement.Domain.Action.Beckn.Issue (mkResContactFields)
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
  let (respondentName, respondentPhone, respondentEmail, resolutionProviderName, resolutionProviderPhone, resolutionProviderEmail) = mkResContactFields igmConfig
   in DBecknIssueStatus.IssueStatusRes
        { issueId = igmIssue.id,
          issueStatus = igmIssue.issueStatus,
          respondentAction = show action,
          groName = igmConfig.groName,
          groPhone = igmConfig.groPhone,
          groEmail = igmConfig.groEmail,
          respondentName = respondentName,
          respondentPhone = respondentPhone,
          respondentEmail = respondentEmail,
          resolutionProviderName = resolutionProviderName,
          resolutionProviderPhone = resolutionProviderPhone,
          resolutionProviderEmail = resolutionProviderEmail,
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
