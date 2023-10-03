{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Ticket where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
-- import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.Ticket as Domain
import EulerHS.KVConnector.Types
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Sequelize
import Tools.Beam.UtilsTH

data TicketT f = TicketT
  { id :: B.C f Text,
    status :: B.C f Domain.TicketStatus,
    quote_id :: B.C f (Maybe Text),
    search_request_id :: B.C f Text,
    bpp_order_id :: B.C f (Maybe Text),
    itemId :: B.C f Text,
    bppTicketId :: B.C f (Maybe Text),
    fulfillmentId :: B.C f (Maybe Text),
    paymentUrl :: B.C f (Maybe Text),
    providerId :: B.C f Text,
    providerUrl :: B.C f Text,
    quantity :: B.C f Integer,
    fromLocationId :: B.C f Text,
    price_per_adult :: B.C f Money,
    total_price :: B.C f Money,
    qr_data :: B.C f (Maybe Text),
    merchantId :: B.C f Text,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketT where
  data PrimaryKey TicketT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Ticket = TicketT Identity

$(enableKVPG ''TicketT ['id] [])

$(mkTableInstances ''TicketT "ticket")
