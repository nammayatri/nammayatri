module Domain.Action.UI.Loyalty
  ( postWalletLoyaltyInfo,
    postRiderMonthlyExpense,
  )
where

import qualified API.Types.UI.Loyalty as LoyaltyAPI
import qualified Data.Time as Time
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment (Flow)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id (Id)
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Utils.Common (fromMaybeM)
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.Payment as SPayment
import qualified Storage.Queries.Person as QPerson
import Tools.Error (PersonError (PersonNotFound))

postWalletLoyaltyInfo ::
  ( ( Maybe (Id Person.Person),
      Id Merchant.Merchant
    ) ->
    Flow LoyaltyAPI.LoyaltyInfoResp
  )
postWalletLoyaltyInfo (mbPersonId, merchantId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "Missing personId in auth context")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  programs <- SPayment.getLoyaltyInfo personId.getId merchantId person.merchantOperatingCityId
  pure LoyaltyAPI.LoyaltyInfoResp {programs}

postRiderMonthlyExpense ::
  ( ( Maybe (Id Person.Person),
      Id Merchant.Merchant
    ) ->
    Flow LoyaltyAPI.MonthlyExpenseResp
  )
postRiderMonthlyExpense (mbPersonId, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "Missing personId in auth context")
  withTime <-
    Redis.runInMasterCloudRedisCellWithCrossAppRedis $
      SWC.getCurrentWindowValuesWithTime (FRFSUtils.riderSpendKey personId) (SWC.SlidingWindowOptions 30 SWC.Days)
  pure
    LoyaltyAPI.MonthlyExpenseResp
      { last30DaysSpend =
          [ LoyaltyAPI.DailySpend {date = Time.utctDay t, amount = fromIntegral (fromMaybe 0 v)}
            | (t, v) <- (withTime :: [(Time.UTCTime, Maybe Integer)])
          ]
      }
