{-
  Finance.Tax.Interface

  Interface for tax calculation (domain provides implementation).
-}
module Lib.Finance.Tax.Interface
  ( ITaxCalculator (..),
  )
where

import Kernel.Prelude
import Lib.Finance.Error.Types
import Lib.Finance.Tax.Types

-- | Tax calculator interface (domain provides implementation with actual rates)
class Monad m => ITaxCalculator m where
  -- | Calculate tax for a taxable item
  calculateTax :: TaxableItem -> m (Either FinanceError TaxBreakdown)

  -- | Get tax rate for a jurisdiction + item type
  getTaxRate :: TaxJurisdiction -> Text -> m (Maybe [TaxComponent])

--                               ^ itemType
