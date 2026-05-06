module Domain.Types.Extra.WalletTransaction
  ( mapWalletStatus,
  )
where

import Domain.Types.WalletTransaction (WalletTransactionStatus (..))
import qualified Kernel.External.Payment.Interface.Types as Payment

-- | Translate a Juspay TransactionStatus to our wallet-domain WalletTransactionStatus.
mapWalletStatus :: Payment.TransactionStatus -> WalletTransactionStatus
mapWalletStatus = \case
  Payment.CHARGED -> SUCCESS
  Payment.AUTO_REFUNDED -> REFUNDED
  Payment.CLIENT_AUTH_TOKEN_EXPIRED -> EXPIRED
  Payment.AUTHENTICATION_FAILED -> FAILED
  Payment.AUTHORIZATION_FAILED -> FAILED
  Payment.JUSPAY_DECLINED -> FAILED
  Payment.CANCELLED -> FAILED
  Payment.STARTED -> PROCESSING
  Payment.PENDING_VBV -> PROCESSING
  Payment.AUTHORIZING -> PROCESSING
  Payment.COD_INITIATED -> PROCESSING
  Payment.NEW -> INITIATED
