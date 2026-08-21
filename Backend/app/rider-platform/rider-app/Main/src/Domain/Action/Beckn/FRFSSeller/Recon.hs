module Domain.Action.Beckn.FRFSSeller.Recon
  ( ReconInput (..),
    ReconDecision (..),
    ReconOutcome (..),
    reconcile,
    settleable,
    counterpartyReconStatus,
    toPaise,
    ReconResult (..),
    EchoIds (..),
    mkResult,
  )
where

import qualified Domain.Types.FRFSRecon as Recon
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney, roundHighPrecMoney)

data ReconInput = ReconInput
  { storedDifference :: Maybe HighPrecMoney,
    storedStatus :: Maybe Recon.ReconStatus,
    storedReference :: Maybe Text,
    incomingAmount :: HighPrecMoney,
    incomingReference :: Maybe Text
  }
  deriving (Show, Eq)

data ReconDecision
  = Apply ReconOutcome
  | AlreadyApplied HighPrecMoney
  | Refused Text
  deriving (Show, Eq)

data ReconOutcome = ReconOutcome
  { newDifference :: HighPrecMoney,
    newStatus :: Recon.ReconStatus
  }
  deriving (Show, Eq)

toPaise :: HighPrecMoney -> HighPrecMoney
toPaise = roundHighPrecMoney 2

reconcile :: ReconInput -> ReconDecision
reconcile input
  | isJust input.incomingReference && input.storedReference == input.incomingReference =
    AlreadyApplied (toPaise (fromMaybe 0 input.storedDifference))
  | otherwise = case input.storedStatus of
    Nothing -> Refused "ledger row has no recon status"
    Just status
      | settleable status -> case input.storedDifference of
        Nothing -> Refused "ledger row has no unsettled balance to reconcile against"
        Just difference ->
          let remaining = toPaise difference - toPaise input.incomingAmount
           in Apply (ReconOutcome {newDifference = remaining, newStatus = statusOf remaining})
      | otherwise -> Refused ("order is not settleable, recon status is " <> show status)
  where
    statusOf remaining
      | remaining == 0 = Recon.SETTLED
      | otherwise = Recon.PARTIALLY_SETTLED

settleable :: Recon.ReconStatus -> Bool
settleable = \case
  Recon.PENDING -> True
  Recon.PARTIALLY_SETTLED -> False
  Recon.SETTLED -> False
  Recon.REFUNDED -> False

counterpartyReconStatus :: HighPrecMoney -> Text
counterpartyReconStatus difference
  | paise == 0 = "01"
  | paise > 0 = "03"
  | otherwise = "02"
  where
    paise = toPaise difference

data ReconResult = ReconResult
  { orderId :: Text,
    difference :: HighPrecMoney,
    wireStatus :: Text,
    note :: Text,
    echo :: EchoIds
  }
  deriving (Show, Eq)

data EchoIds = EchoIds
  { echoTransactionId :: Maybe Text,
    echoSettlementId :: Maybe Text,
    echoSettlementReference :: Maybe Text
  }
  deriving (Show, Eq)

mkResult :: EchoIds -> Text -> HighPrecMoney -> Text -> ReconResult
mkResult echo orderId difference note =
  ReconResult {orderId, difference, wireStatus = counterpartyReconStatus difference, note, echo}
