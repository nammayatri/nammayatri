{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Extra.PurchasedPass where

import Control.Lens.Operators
import Data.Aeson
import Data.OpenApi
import qualified Data.Text as T
import qualified Domain.Types.PurchasedPass
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Utils.TH

allStatusTypes :: [Domain.Types.PurchasedPass.StatusType]
allStatusTypes = [Domain.Types.PurchasedPass.Pending, Domain.Types.PurchasedPass.Active, Domain.Types.PurchasedPass.Failed, Domain.Types.PurchasedPass.Expired, Domain.Types.PurchasedPass.RefundPending, Domain.Types.PurchasedPass.RefundInitiated, Domain.Types.PurchasedPass.Refunded]

$(mkBeamInstancesForEnum ''Domain.Types.PurchasedPass.BenefitType)
$(mkBeamInstancesForEnum ''Domain.Types.PurchasedPass.StatusType)
$(mkHttpInstancesForEnum ''Domain.Types.PurchasedPass.StatusType)

instance ToParamSchema Domain.Types.PurchasedPass.StatusType where
  toParamSchema _ =
    mempty
      & title ?~ "StatusType"
      & type_ ?~ OpenApiString
      & enum_
        ?~ map (String . T.pack . show) allStatusTypes
