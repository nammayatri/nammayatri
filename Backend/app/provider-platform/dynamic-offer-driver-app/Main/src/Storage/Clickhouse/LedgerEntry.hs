{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Clickhouse.LedgerEntry where

import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.Account as FAccount
import qualified Lib.Finance.Domain.Types.LedgerEntry as DLE

data LedgerEntryT f = LedgerEntryT
  { id :: C f (Id DLE.LedgerEntry),
    fromAccountId :: C f (Id FAccount.Account),
    toAccountId :: C f (Id FAccount.Account),
    amount :: C f HighPrecMoney,
    referenceType :: C f Text,
    referenceId :: C f Text,
    concernedIndividualId :: C f (Maybe Text),
    timestamp :: C f UTCTime
  }
  deriving (Generic)

ledgerEntryTTable :: LedgerEntryT (FieldModification LedgerEntryT)
ledgerEntryTTable =
  LedgerEntryT
    { id = "id",
      fromAccountId = "from_account_id",
      toAccountId = "to_account_id",
      amount = "amount",
      referenceType = "reference_type",
      referenceId = "reference_id",
      concernedIndividualId = "concerned_individual_id",
      timestamp = "timestamp"
    }

type LedgerEntry = LedgerEntryT Identity

$(TH.mkClickhouseInstances ''LedgerEntryT 'NO_SELECT_MODIFIER)

-- | Minimal row shape consumed by the wallet summary aggregator.
data WalletEntryRow = WalletEntryRow
  { walletAmount :: HighPrecMoney,
    walletToAccountId :: Id FAccount.Account,
    walletReferenceType :: Text,
    walletTimestamp :: UTCTime
  }
  deriving (Show)

findWalletEntries ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id FAccount.Account] ->
  Maybe Text -> -- concernedIndividualId filter (fleet driver under fleet owner)
  UTCTime ->
  UTCTime ->
  [Text] -> -- referenceTypes filter
  m [WalletEntryRow]
findWalletEntries accountIds mbConcerned fromDate toDate_ refs = do
  rows <-
    CH.findAll $
      CH.select_
        (\e -> CH.notGrouped (e.amount, e.toAccountId, e.referenceType, e.timestamp))
        $ CH.filter_
          ( \e ->
              ( e.fromAccountId `in_` accountIds
                  CH.||. e.toAccountId `in_` accountIds
              )
                CH.&&. e.referenceType `in_` refs
                CH.&&. e.timestamp >=. fromDate
                CH.&&. e.timestamp <=. toDate_
                CH.&&. CH.whenJust_ mbConcerned (\cid -> e.concernedIndividualId CH.==. Just cid)
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE ledgerEntryTTable)
  pure $ map mkRow rows
  where
    mkRow (amount, toAccountId, referenceType, ts) =
      WalletEntryRow
        { walletAmount = amount,
          walletToAccountId = toAccountId,
          walletReferenceType = referenceType,
          walletTimestamp = ts
        }
