{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.Dashboard.AppManagement.HotSpot (postHotSpotRemoveExpired) where

import qualified Domain.Action.UI.HotSpot as DAH
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import SharedLogic.Merchant

postHotSpotRemoveExpired ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postHotSpotRemoveExpired merchantShortId _opCity = do
  m <- findMerchantByShortId merchantShortId
  DAH.removeExpiredHotSpots m.id
