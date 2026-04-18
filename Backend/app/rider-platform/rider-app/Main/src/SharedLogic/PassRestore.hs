{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Pass restoration logic for re-registered users.
--
-- When a user deletes their account and re-registers with the same mobile
-- number, this module migrates all their purchased passes (and associated
-- payment orders) from the old personId to the new one. The migration is
-- triggered asynchronously from the auth-verify handler so that login is
-- never blocked.
module SharedLogic.PassRestore (restorePurchasedPassesIfNeeded) where

import qualified Domain.Types.Person as SP
import qualified Domain.Types.PurchasedPass as DPP
import EulerHS.Prelude hiding (id)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Utils.Common
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified SharedLogic.Utils as SLUtils
import Storage.Beam.Payment ()
import qualified Storage.Queries.DeletedPerson as QDeletedPerson
import qualified Storage.Queries.PurchasedPass as QPurchasedPass
import qualified Storage.Queries.PurchasedPassPayment as QPurchasedPassPayment

-- | Restore purchased passes for a newly-registered person if they previously
-- had an account with the same mobile number.
--
-- Algorithm:
--   Find the first (newest) deleted account with Active/PreBooked passes.
--   Migrate all qualifying passes from that account only, then return.
--   Older accounts are only checked if the newer one has no qualifying passes.
--
-- Guard: skipped entirely if the new person already has passes (prevents
-- duplicate processing on retries / repeated re-registrations).
restorePurchasedPassesIfNeeded ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    MonadFlow m,
    CacheFlow m r
  ) =>
  -- | The newly-registered person (migration target) and their decrypted mobile number
  SP.Person ->
  Text ->
  m ()
restorePurchasedPassesIfNeeded newPerson mobileNumber = do
  -- Guard: if newPerson already has passes, skip to prevent duplicate passes
  -- on retries or repeated re-registrations.
  existingPasses <- QPurchasedPass.findAllByPersonId newPerson.id
  unless (null existingPasses) $ do
    logInfo $ "PassRestore: person already has passes, skipping migration for newPersonId=" <> newPerson.id.getId

  -- Derive the same static customer ID that was stored at deletion time.
  staticPersonId <- SLUtils.getStaticCustomerId newPerson mobileNumber
  deletedPersons <- QDeletedPerson.findAllByStaticPersonId Nothing Nothing (Just staticPersonId)

  -- Single-migration: process only the first deleted account with qualifying passes
  migrateFromFirstMatching newPerson.id deletedPersons
  where
    migrateFromFirstMatching _ [] =
      logInfo $ "PassRestore: no qualifying passes found for newPersonId=" <> newPerson.id.getId

    migrateFromFirstMatching newPersonId (deletedPerson : rest) = do
      let oldPersonId = deletedPerson.personId
      passes <- QPurchasedPass.findAllByPersonId oldPersonId
      let toMigrate = filter (\p -> p.status == DPP.Active || p.status == DPP.PreBooked) passes

      if null toMigrate
        then do
          logInfo $ "PassRestore: no qualifying passes for oldPersonId=" <> oldPersonId.getId <> ", checking older accounts"
          migrateFromFirstMatching newPersonId rest
        else do
          logInfo $ "PassRestore: migrating " <> show (length toMigrate) <> " pass(es) from oldPersonId=" <> oldPersonId.getId
          traverse_ (migratePass newPersonId) toMigrate
          logInfo $ "PassRestore: migration complete for newPersonId=" <> newPersonId.getId
    -- Return early after first successful migration

    migratePass newPersonId purchasedPass = do
      payments <- QPurchasedPassPayment.findAllByPurchasedPassId purchasedPass.id
      traverse_ (\p -> QPaymentOrder.updatePersonId p.orderId newPersonId.getId) payments
      QPurchasedPassPayment.updatePersonIdByPurchasedPassId newPersonId purchasedPass.id
      QPurchasedPass.updatePersonIdById newPersonId purchasedPass.id
