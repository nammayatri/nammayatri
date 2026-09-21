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
  -- The reference is the ONLY thing that makes applying a settlement idempotent, and it is
  -- optional on the wire. Without it a retried cycle cannot be told from a genuine second
  -- instalment, so a replay past the 60s dedupe TTL would debit the balance twice and end up
  -- answering "01"/reconciled on money never received. Refuse loudly instead of guessing.
  | isNothing input.incomingReference =
    Refused "settlement carries no settlement_reference_no, so it cannot be applied idempotently"
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
    -- <= 0, not == 0. An overpayment leaves a NEGATIVE balance; calling that PARTIALLY_SETTLED
    -- would keep the row settleable and let later cycles drive it further negative, and the
    -- exact `== 0` test could never bring it back. Fully paid is terminal either way -- the
    -- excess is what 'counterpartyReconStatus' reports as "02".
    statusOf remaining
      | remaining <= 0 = Recon.SETTLED
      | otherwise = Recon.PARTIALLY_SETTLED

-- | PARTIALLY_SETTLED must stay settleable: 'statusOf' produces it whenever a cycle leaves a
-- balance, so refusing it here would strand the order and keep answering "03" (short) against
-- a collector that goes on to pay the rest in a later cycle.
settleable :: Recon.ReconStatus -> Bool
settleable = \case
  Recon.PENDING -> True
  Recon.PARTIALLY_SETTLED -> True
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
