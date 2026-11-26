{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Ops notes on a scheduled booking: comments on the one issue report kept per scheduled transaction, shown on the
-- scheduled-booking ops screens. The dashboard writes the notes an operator types; the system writes its own (a
-- shared-route run nobody covers, a run cancelled at departure) under 'scheduledBookingIssuePersonId'.
--
-- The dashboard creates the transaction's issue report through the issue module's full creation flow, which needs the
-- driver app's own environment. A system note can come from the allocator too, so when no report exists yet it writes
-- the same row that flow would write for this category (no ticket, no chats, status NOT_APPLICABLE) directly. Both go
-- through 'addOpsNoteWith' under one lock per transaction, so they never create two reports.
module SharedLogic.ScheduledBooking.OpsNote
  ( OpsNoteFlow,
    addOpsNoteWith,
    addOpsNote,
    addSystemOpsNote,
    scheduledBookingIssuePersonId,
    scheduledBookingIssueDriverId,
    scheduledBookingIssueCategoryId,
    scheduledBookingIssueDescription,
    maxOpsNoteLength,
    opsNoteLockKey,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified IssueManagement.Common as Issue
import qualified IssueManagement.Domain.Types.Issue.Comment as DComment
import qualified IssueManagement.Domain.Types.Issue.IssueCategory as DIssueCategory
import qualified IssueManagement.Domain.Types.Issue.IssueReport as DIssueReport
import qualified IssueManagement.Storage.Queries.Issue.Comment as QComment
import qualified IssueManagement.Storage.Queries.Issue.IssueReport as QIssueReport
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.IssueManagement ()

type OpsNoteFlow m r = (MonadFlow m, CacheFlow m r, EsqDBFlow m r)

-- | The author of system notes and the person the transaction's issue report is filed under.
scheduledBookingIssuePersonId :: Id DP.Person
scheduledBookingIssuePersonId = Id "00000000-0000-0000-0000-000000000000"

scheduledBookingIssueDriverId :: Id Issue.Person
scheduledBookingIssueDriverId = Id "driver-0000000-0000-1"

scheduledBookingIssueCategoryId :: Id DIssueCategory.IssueCategory
scheduledBookingIssueCategoryId = Id "Schedule_Booking_Issue"

scheduledBookingIssueDescription :: Text
scheduledBookingIssueDescription = "Scheduled booking operations notes"

maxOpsNoteLength :: Int
maxOpsNoteLength = 255

opsNoteLockKey :: Id DMOC.MerchantOperatingCity -> Text -> Text
opsNoteLockKey merchantOperatingCityId transactionId =
  "ScheduledBookingOpsNote:" <> merchantOperatingCityId.getId <> ":" <> transactionId

-- | One note on the booking's transaction, by @authorId@, with the transaction's report found or created by
-- @getOrCreateReport@ under the transaction's lock.
addOpsNoteWith :: OpsNoteFlow m r => (DRB.Booking -> m (Id DIssueReport.IssueReport)) -> DRB.Booking -> Id DP.Person -> Text -> m ()
addOpsNoteWith getOrCreateReport booking authorId note =
  Redis.withWaitAndLockRedisWithTimeout (opsNoteLockKey booking.merchantOperatingCityId booking.transactionId) 10 10 50000 $ do
    issueReportId <- getOrCreateReport booking
    commentId <- generateGUID
    now <- getCurrentTime
    QComment.create
      DComment.Comment
        { id = commentId,
          issueReportId,
          authorId = cast authorId,
          comment = note,
          createdAt = now,
          merchantId = Just $ cast booking.providerId
        }

-- | A note written by the platform rather than an operator; callable from the driver app and the allocator alike.
addOpsNote :: OpsNoteFlow m r => DRB.Booking -> Id DP.Person -> Text -> m ()
addOpsNote = addOpsNoteWith findOrCreateSystemIssueReport

-- | 'addOpsNote' by the system author, cut to the ops-note length limit.
addSystemOpsNote :: OpsNoteFlow m r => DRB.Booking -> Text -> m ()
addSystemOpsNote booking note = addOpsNote booking scheduledBookingIssuePersonId (T.take maxOpsNoteLength note)

-- | The transaction's report, or the row the issue module's creation flow writes for this category when there is none.
findOrCreateSystemIssueReport :: OpsNoteFlow m r => DRB.Booking -> m (Id DIssueReport.IssueReport)
findOrCreateSystemIssueReport booking =
  QIssueReport.findByScheduledBookingTransactionId (cast booking.merchantOperatingCityId) booking.transactionId >>= \case
    Just issueReport -> pure issueReport.id
    Nothing -> do
      id <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      QIssueReport.create
        DIssueReport.IssueReport
          { id,
            shortId = Just shortId,
            personId = cast scheduledBookingIssuePersonId,
            driverId = Just scheduledBookingIssueDriverId,
            rideId = Nothing,
            ticketBookingId = Nothing,
            scheduledBookingTransactionId = Just booking.transactionId,
            merchantOperatingCityId = Just (cast booking.merchantOperatingCityId),
            description = scheduledBookingIssueDescription,
            assignee = Nothing,
            status = Issue.NOT_APPLICABLE,
            categoryId = Just scheduledBookingIssueCategoryId,
            categoryName = Nothing,
            optionId = Nothing,
            deleted = False,
            mediaFiles = [],
            createdAt = now,
            updatedAt = now,
            ticketId = Nothing,
            additionalTicketIds = Nothing,
            chats = [],
            merchantId = Just (cast booking.providerId),
            becknIssueId = Nothing,
            reopenedCount = 0,
            customerResponse = Nothing,
            submitLabel = Nothing,
            ticketFormFields = Nothing
          }
      pure id
