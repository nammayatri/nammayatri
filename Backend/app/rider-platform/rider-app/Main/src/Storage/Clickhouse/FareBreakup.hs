{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Clickhouse.FareBreakup where

import Control.Lens ((^?), _head)
import qualified Domain.Types.Booking as DB
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common

data FareBreakupT f = FareBreakupT
  { bookingId :: C f (Id DB.Booking),
    description :: C f Text,
    amount :: C f (Maybe HighPrecMoney),
    date :: C f UTCTime
  }
  deriving (Generic)

deriving instance Show FareBreakup

-- TODO move to TH (quietSnake)
fareBreakupTTable :: FareBreakupT (FieldModification FareBreakupT)
fareBreakupTTable =
  FareBreakupT
    { bookingId = "booking_id",
      description = "description",
      amount = "amount",
      date = "date"
    }

type FareBreakup = FareBreakupT Identity

$(TH.mkClickhouseInstances ''FareBreakupT 'SELECT_FINAL_MODIFIER)

findFareBreakupByBookingIdAndDescription ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DB.Booking ->
  Text ->
  UTCTime ->
  m (Maybe FareBreakup)
findFareBreakupByBookingIdAndDescription bookingId description createdAt = do
  fareBreakup <-
    CH.findAll $
      CH.select $
        CH.filter_
          ( \fareBreakup ->
              fareBreakup.bookingId CH.==. bookingId
                CH.&&. fareBreakup.description CH.==. description
                CH.&&. fareBreakup.date >=. addUTCTime (-120) createdAt
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fareBreakupTTable)
  return $ fareBreakup ^? _head
