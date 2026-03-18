module Domain.Action.Dashboard.Operator.Payout
  ( getOperatorPayoutReceived,
    getOperatorPayoutReceivedTimeSeries,
    getOperatorPayoutReceivedByFleet,
    getOperatorPayoutOutstanding,
    getOperatorPayoutOutstandingBreakdown,
  )
where

import qualified API.Types.ProviderPlatform.Operator.Payout as Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common (HighPrecMoney (..))
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common
import qualified Storage.Queries.DriverFeeExtra as QDF
import qualified Storage.Queries.DriverOperatorAssociationExtra as QDOA
import qualified Storage.Queries.FleetOperatorAssociationExtra as QFOA
import qualified Storage.Queries.Person as QP
import Tools.Error

---------------------------------------------------------------------
-- GET /operator/payout/received
---------------------------------------------------------------------
getOperatorPayoutReceived ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Text ->
  Environment.Flow Common.OperatorReceivedPayoutRes
getOperatorPayoutReceived _merchantShortId _opCity mbFrom mbTo requestorId = do
  person <- QP.findById (ID.Id requestorId) >>= fromMaybeM (PersonNotFound requestorId)
  unless (person.role == DP.OPERATOR) $ throwError (InvalidRequest "Requestor role is not OPERATOR")
  driverIds <- QDOA.getActiveDriverIdsByOperatorId requestorId
  let driverIdTexts = map (.getId) driverIds
  (totalReceived, rideCommissions, subscriptionShare, _firstDate, _lastDate) <-
    QDF.getSettledAmountsForDriverIds driverIdTexts mbFrom mbTo
  let incentives = HighPrecMoney 0
      deductions = HighPrecMoney 0
      netReceived = totalReceived
  pure
    Common.OperatorReceivedPayoutRes
      { totalReceived = totalReceived,
        currency = INR,
        rideCommissionsReceived = rideCommissions,
        subscriptionShareReceived = subscriptionShare,
        incentivesReceived = incentives,
        deductionsApplied = deductions,
        netReceived = netReceived,
        periodFrom = mbFrom,
        periodTo = mbTo
      }

---------------------------------------------------------------------
-- GET /operator/payout/receivedTimeSeries
---------------------------------------------------------------------
getOperatorPayoutReceivedTimeSeries ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.UTCTime ->
  Kernel.Prelude.UTCTime ->
  Common.Granularity ->
  Kernel.Prelude.Text ->
  Environment.Flow Common.OperatorReceivedTimeSeriesRes
getOperatorPayoutReceivedTimeSeries _merchantShortId _opCity from to granularity requestorId = do
  person <- QP.findById (ID.Id requestorId) >>= fromMaybeM (PersonNotFound requestorId)
  unless (person.role == DP.OPERATOR) $ throwError (InvalidRequest "Requestor role is not OPERATOR")
  driverIds <- QDOA.getActiveDriverIdsByOperatorId requestorId
  let driverIdTexts = map (.getId) driverIds
      granStr = case granularity of
        Common.DAILY -> "day"
        Common.WEEKLY -> "week"
        Common.MONTHLY -> "month"
  buckets <- QDF.getSettledAmountsTimeSeries driverIdTexts from to granStr
  let total = foldl' (\acc b -> acc + b.amount) (HighPrecMoney 0) buckets
  pure
    Common.OperatorReceivedTimeSeriesRes
      { buckets = buckets,
        granularity = granularity,
        total = total
      }

---------------------------------------------------------------------
-- GET /operator/payout/receivedByFleet
---------------------------------------------------------------------
getOperatorPayoutReceivedByFleet ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Text ->
  Environment.Flow Common.OperatorReceivedByFleetRes
getOperatorPayoutReceivedByFleet _merchantShortId _opCity mbFrom mbTo mbLimit mbOffset requestorId = do
  person <- QP.findById (ID.Id requestorId) >>= fromMaybeM (PersonNotFound requestorId)
  unless (person.role == DP.OPERATOR) $ throwError (InvalidRequest "Requestor role is not OPERATOR")
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  fleetAssociations <- QFOA.findAllActiveByOperatorIdWithLimitOffset requestorId limit offset
  items <- forM fleetAssociations $ \foa -> do
    fleetOwner <- QP.findById (ID.Id foa.fleetOwnerId) >>= fromMaybeM (PersonNotFound foa.fleetOwnerId)
    decMobileNumber <- mapM decrypt fleetOwner.mobileNumber
    fleetDriverIds <- QDOA.getActiveDriverIdsByOperatorId foa.fleetOwnerId
    let driverIdTexts = map (.getId) fleetDriverIds
    (totalRcvd, _rideComm, _subShare, firstDate, lastDate) <-
      QDF.getSettledAmountsForDriverIds driverIdTexts mbFrom mbTo
    pure
      Common.FleetReceivedItem
        { fleetOwnerId = ID.Id foa.fleetOwnerId,
          fleetOwnerName = fromMaybe "" fleetOwner.firstName,
          mobileNumber = fromMaybe "" decMobileNumber,
          totalReceived = totalRcvd,
          driverCount = length fleetDriverIds,
          firstPayoutDate = firstDate,
          lastPayoutDate = lastDate
        }
  let count = length items
      summary = Common.Summary {totalCount = 10000, count}
  pure Common.OperatorReceivedByFleetRes {items, summary}

---------------------------------------------------------------------
-- GET /operator/payout/outstanding
---------------------------------------------------------------------
getOperatorPayoutOutstanding ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Text ->
  Environment.Flow Common.OperatorOutstandingPayoutRes
getOperatorPayoutOutstanding _merchantShortId _opCity _mbFleetOwnerId _mbFrom _mbTo requestorId = do
  person <- QP.findById (ID.Id requestorId) >>= fromMaybeM (PersonNotFound requestorId)
  unless (person.role == DP.OPERATOR) $ throwError (InvalidRequest "Requestor role is not OPERATOR")
  now <- getCurrentTime
  driverIds <- QDOA.getActiveDriverIdsByOperatorId requestorId
  let driverIdTexts = map (.getId) driverIds
  (totalOutstanding, rideCommissions, subscriptionShare) <-
    QDF.getPendingAmountsForDriverIds driverIdTexts
  pure
    Common.OperatorOutstandingPayoutRes
      { totalOutstanding = totalOutstanding,
        currency = INR,
        rideCommissionPending = rideCommissions,
        subscriptionSharePending = subscriptionShare,
        incentivesPending = HighPrecMoney 0,
        deductions = HighPrecMoney 0,
        lastUpdatedAt = now
      }

---------------------------------------------------------------------
-- GET /operator/payout/outstandingBreakdown
---------------------------------------------------------------------
getOperatorPayoutOutstandingBreakdown ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Text ->
  Environment.Flow Common.OperatorOutstandingBreakdownRes
getOperatorPayoutOutstandingBreakdown _merchantShortId _opCity mbLimit mbOffset requestorId = do
  person <- QP.findById (ID.Id requestorId) >>= fromMaybeM (PersonNotFound requestorId)
  unless (person.role == DP.OPERATOR) $ throwError (InvalidRequest "Requestor role is not OPERATOR")
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  fleetAssociations <- QFOA.findAllActiveByOperatorIdWithLimitOffset requestorId limit offset
  items <- forM fleetAssociations $ \foa -> do
    fleetOwner <- QP.findById (ID.Id foa.fleetOwnerId) >>= fromMaybeM (PersonNotFound foa.fleetOwnerId)
    decMobileNumber <- mapM decrypt fleetOwner.mobileNumber
    fleetDriverIds <- QDOA.getActiveDriverIdsByOperatorId foa.fleetOwnerId
    let driverIdTexts = map (.getId) fleetDriverIds
    (outstandingAmt, _rideComm, _subShare) <-
      QDF.getPendingAmountsForDriverIds driverIdTexts
    pure
      Common.FleetOwnerOutstandingItem
        { fleetOwnerId = ID.Id foa.fleetOwnerId,
          fleetOwnerName = fromMaybe "" fleetOwner.firstName,
          mobileNumber = fromMaybe "" decMobileNumber,
          outstandingAmount = outstandingAmt,
          driverCount = length fleetDriverIds,
          lastPayoutDate = Nothing
        }
  let count = length items
      summary = Common.Summary {totalCount = 10000, count}
  pure Common.OperatorOutstandingBreakdownRes {items, summary}
