{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Lib.Payment.Wallet.Types
  ( LoyaltyProgramSummary (..),
    toLoyaltyProgramSummary,
  )
where

import qualified Kernel.External.Wallet.Interface.Types as WalletTypes
import Kernel.Prelude

data LoyaltyProgramSummary = LoyaltyProgramSummary
  { id :: Text,
    programType :: Maybe Text,
    burn :: WalletTypes.BurnInfo,
    earn :: Maybe WalletTypes.EarnInfo,
    uiLabel :: Maybe WalletTypes.ProgramUiLabel,
    wallet :: WalletTypes.ProgramWallet
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

toLoyaltyProgramSummary :: (Text -> Maybe Text) -> WalletTypes.LoyaltyInfoProgram -> LoyaltyProgramSummary
toLoyaltyProgramSummary resolveProgramType p =
  LoyaltyProgramSummary
    { id = p.id_,
      programType = resolveProgramType p.id_,
      burn = p.burn,
      earn = p.earn,
      uiLabel = p.uiLabel,
      wallet = p.wallet
    }
