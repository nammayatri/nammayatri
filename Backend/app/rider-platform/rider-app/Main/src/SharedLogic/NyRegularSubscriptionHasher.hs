module SharedLogic.NyRegularSubscriptionHasher
  ( calculateSubscriptionSchedulingHash,
  )
where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Domain.Types.NyRegularSubscription as NySub
import Kernel.External.Encryption (DbHash, EncFlow, getDbHash)
import Kernel.Prelude
import Kernel.Types.App (MonadFlow)

-- | Calculates a DbHash representing the scheduling-critical state of a NyRegularSubscription.
-- This hash is used to detect if a subscription's scheduling parameters have changed
-- since a job instance was created for it.
calculateSubscriptionSchedulingHash ::
  (MonadFlow m, EncFlow m r) => -- Assuming EncFlow is needed for getDbHash context
  NySub.NyRegularSubscription ->
  m DbHash
calculateSubscriptionSchedulingHash sub = do
  let -- Canonical representation of DayOfWeek list
      recurrenceDaysText =
        sub.recurrenceRuleDays
          & List.sort -- Sort for consistent order
          & map (T.pack . show) -- Convert each DayOfWeek to Text
          & T.intercalate "," -- Join with a comma

      -- Canonical representation of TimeOfDay
      timeOfDayText =
        T.pack $ Time.formatTime Time.defaultTimeLocale "%H:%M:%S" sub.scheduledTimeOfDay

      -- Canonical representation of Day (for recurrenceEndDate)
      -- Using ISO 8601 YYYY-MM-DD format
      endDateText =
        maybe "none" (T.pack . Time.formatTime Time.defaultTimeLocale "%Y-%m-%d") sub.recurrenceEndDate

      -- Canonical representation of UTCTime
      -- Using ISO 8601 YYYY-MM-DDTHH:MM:SSZ format
      startDatetimeText =
        T.pack $ Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" sub.startDatetime

      pauseStartDateText =
        maybe "none" (T.pack . Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ") sub.pauseStartDate

      pauseEndDateText =
        maybe "none" (T.pack . Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ") sub.pauseEndDate

      statusText =
        T.pack $ show sub.status

      idText =
        sub.id.getId -- Get the Text representation of the Id

      -- Concatenate all relevant fields with a consistent delimiter.
      -- The order of fields here is important and must be maintained.
      serializedData =
        T.intercalate
          "|"
          [ "id:" <> idText,
            "status:" <> statusText,
            "start_datetime:" <> startDatetimeText,
            "recurrence_days:" <> recurrenceDaysText,
            "scheduled_time:" <> timeOfDayText,
            "end_date:" <> endDateText,
            "pause_start:" <> pauseStartDateText,
            "pause_end:" <> pauseEndDateText
          ]

  -- For debugging, can be uncommented if needed during development:
  -- logDebug $ "Serialized NyRegularSubscription data for hashing (" <> sub.id.getId <> "): " <> serializedData
  getDbHash serializedData
