{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Extra.PurchasedPass where

import qualified Domain.Types.PurchasedPass

allStatusTypes :: [Domain.Types.PurchasedPass.StatusType]
allStatusTypes = [Domain.Types.PurchasedPass.Pending, Domain.Types.PurchasedPass.Active, Domain.Types.PurchasedPass.Failed, Domain.Types.PurchasedPass.Expired, Domain.Types.PurchasedPass.RefundPending, Domain.Types.PurchasedPass.RefundInitiated, Domain.Types.PurchasedPass.Refunded]
