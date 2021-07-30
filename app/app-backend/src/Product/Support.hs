{-# LANGUAGE OverloadedStrings #-}

module Product.Support
  ( sendIssue,
  )
where

import qualified App.Types as App
import qualified Beckn.SesConfig as SesConfig
import Beckn.Types.APISuccess
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Validation (runRequestValidation)
import Data.Time (UTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (length)
import qualified Storage.Queries.Issues as Queries
import Types.API.Support as Support
import qualified Types.Storage.Issue as SIssue
import Types.Storage.Person as Person
import Utils.Common
import qualified Utils.SES as SES

sendIssue :: Id Person.Person -> Support.SendIssueReq -> App.FlowHandler Support.SendIssueRes
sendIssue personId request@SendIssueReq {..} = withFlowHandlerAPI $ do
  runRequestValidation validateSendIssueReq request
  let personIdTxt = getId personId
  issuesConfig <- asks $ SesConfig.issuesConfig . App.sesCfg
  issueId <- L.generateGUID
  utcNow <- getCurrentTime
  Queries.insertIssue (mkDBIssue issueId personIdTxt request utcNow)
  let mailSubject = mkMailSubject issueId (issue.reason)
  let mailBody = mkMailBody issueId personIdTxt request utcNow
  responseError <- L.runIO $ SES.sendEmail issuesConfig mailSubject mailBody
  whenJust responseError $ throwError . EmailSendingError
  return Success

mkDBIssue :: Text -> Text -> Support.SendIssueReq -> UTCTime -> SIssue.Issue
mkDBIssue issueId customerId SendIssueReq {..} time =
  SIssue.Issue
    { id = Id issueId,
      customerId = Id customerId,
      productInstanceId = Id <$> productInstanceId,
      contactEmail = contactEmail,
      reason = issue.reason,
      description = issue.description,
      createdAt = time,
      updatedAt = time
    }

mkMailSubject :: Text -> Text -> Text
mkMailSubject issueId reason = "Issue " <> issueId <> ". " <> reason

mkMailBody :: Text -> Text -> Support.SendIssueReq -> UTCTime -> Text
mkMailBody issueId personId SendIssueReq {..} time =
  "Issue id: " <> issueId
    <> "\nPerson id: "
    <> personId
    <> "\nOrder id: "
    <> orderId
    <> "\nContact email: "
    <> contactEmail
    <> "\nCreation time: "
    <> show time
    <> "\n\nReason: "
    <> issue.reason
    <> "\n\nDetails: "
    <> description
  where
    orderId = fromMaybe "issue does not belong to specific order." productInstanceId
    description = fromMaybe "no details provided by user." (issue.description)
