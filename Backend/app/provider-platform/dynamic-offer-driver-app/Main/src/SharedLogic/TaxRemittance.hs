{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.TaxRemittance
  ( TaxAccounts (..),
    taxAccountsFor,
    resolveMode,
    legacyRideMode,
  )
where

import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Lib.Finance (AccountRole (..))

data TaxAccounts = TaxAccounts
  { forwardDest :: AccountRole,
    refundSource :: AccountRole,
    benefitForwardDest :: AccountRole,
    benefitRefundSource :: AccountRole
  }
  deriving (Eq, Show)

taxAccountsFor :: DTC.TaxRemittanceMode -> TaxAccounts
taxAccountsFor DTC.DRIVER_DIRECT =
  TaxAccounts
    { forwardDest = OwnerLiability,
      refundSource = OwnerLiability,
      benefitForwardDest = SellerRevenue,
      benefitRefundSource = SellerExpense
    }
taxAccountsFor DTC.COMPANY_DIRECT =
  TaxAccounts
    { forwardDest = GovtIndirect,
      refundSource = GovtIndirect,
      benefitForwardDest = GovtIndirect,
      benefitRefundSource = GovtIndirect
    }
taxAccountsFor DTC.COMPANY_INDIRECT =
  TaxAccounts
    { forwardDest = OwnerLiability,
      refundSource = GovtIndirect,
      benefitForwardDest = GovtIndirect,
      benefitRefundSource = GovtIndirect
    }

resolveMode :: Maybe DTC.TaxRemittanceMode -> Maybe DTC.TaxRemittanceMode -> DTC.TaxRemittanceMode -> DTC.TaxRemittanceMode
resolveMode (Just m) _ _ = m
resolveMode Nothing (Just m) _ = m
resolveMode Nothing Nothing legacyDefault = legacyDefault

legacyRideMode :: Maybe Bool -> DTC.TaxRemittanceMode
legacyRideMode mbIsVat = if fromMaybe False mbIsVat then DTC.DRIVER_DIRECT else DTC.COMPANY_DIRECT
