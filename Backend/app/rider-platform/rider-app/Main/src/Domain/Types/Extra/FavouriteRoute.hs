module Domain.Types.Extra.FavouriteRoute where

-- recurrenceRuleDays is [Data.Time.Calendar.DayOfWeek], which has no ToSchema of its
-- own. The orphan instance lives in Domain.Types.Extra.NyRegularSubscription (that
-- table uses the same type for the same purpose); importing it for instances only
-- brings it into scope here. It must not be redefined -- two orphan instances for the
-- same type would clash.
import Domain.Types.Extra.NyRegularSubscription ()
