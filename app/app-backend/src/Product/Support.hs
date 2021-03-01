{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Product.Support
  ( sendIssue,
  )
where

import qualified App.Types as App
import qualified Beckn.SesConfig as SesConfig
import Beckn.Types.App (IssueId (..))
import Beckn.Types.ID
import qualified Beckn.Types.Storage.Issue as SIssue
import Beckn.Types.Storage.Person as Person
import Beckn.Utils.Common (getCurrTime, withFlowHandler)
import Data.Time (UTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (length)
import qualified Storage.Queries.Issues as Queries
import qualified Types.API.Common as API
import Types.API.Support as Support
import qualified Utils.SES as SES

sendIssue :: Person.Person -> Support.SendIssueReq -> App.FlowHandler Support.SendIssueRes
sendIssue person request@SendIssueReq {..} = withFlowHandler $ do
  let personId = getId $ person ^. #_id
  issuesConfig <- asks $ SesConfig.issuesConfig . App.sesCfg
  issueId <- L.generateGUID
  utcNow <- getCurrTime
  Queries.insertIssue (mkDBIssue issueId personId request utcNow)
  let mailSubject = mkMailSubject issueId (issue ^. #_reason)
  let mailBody = mkMailBody issueId personId request utcNow
  responseError <- L.runIO $ SES.sendEmail issuesConfig mailSubject mailBody
  case responseError of
    Just resError -> pure $ API.Ack {_action = "Error", _message = resError}
    Nothing -> pure $ API.Ack {_action = "Successful", _message = ""}

mkDBIssue :: Text -> Text -> Support.SendIssueReq -> UTCTime -> SIssue.Issue
mkDBIssue issueId customerId SendIssueReq {..} time =
  SIssue.Issue
    { _id = IssueId issueId,
      _customerId = customerId,
      _productInstanceId = productInstanceId,
      _contactEmail = contactEmail,
      _reason = issue ^. #_reason,
      _description = issue ^. #_description,
      _createdAt = time,
      _updatedAt = time
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
    <> issue ^. #_reason
    <> "\n\nDetails: "
    <> description
  where
    orderId = fromMaybe "issue does not belong to specific order." productInstanceId
    description = fromMaybe "no details provided by user." (issue ^. #_description)
