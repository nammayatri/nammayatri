{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Extra.PurchasedPass where

import qualified Domain.Types.PurchasedPass
import Kernel.Beam.Lib.UtilsTH

$(mkBeamInstancesForEnum ''Domain.Types.PurchasedPass.BenefitType)
$(mkBeamInstancesForEnum ''Domain.Types.PurchasedPass.StatusType)
