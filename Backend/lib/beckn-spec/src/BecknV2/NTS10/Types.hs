module BecknV2.NTS10.Types where

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import EulerHS.Prelude
import qualified Text.Read as TR

newtype FlexAmount = FlexAmount {getFlexAmount :: Text}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON FlexAmount where
  parseJSON = \case
    Null -> pure (FlexAmount "")
    String s -> pure (FlexAmount s)
    Number n ->
      pure . FlexAmount . decodeUtf8 . BSL.toStrict $ encode (Number n)
    v -> typeMismatch "FlexAmount (string or number)" v

instance ToJSON FlexAmount where
  toJSON (FlexAmount t) = String t

flexAmountToDouble :: FlexAmount -> Maybe Double
flexAmountToDouble (FlexAmount t) = TR.readMaybe (T.unpack (T.strip t))

data ReconContext = ReconContext
  { reconContextDomain :: Maybe Text,
    reconContextCountry :: Maybe Text,
    reconContextCity :: Maybe Text,
    reconContextAction :: Maybe Text,
    reconContextCoreVersion :: Maybe Text,
    reconContextBapId :: Maybe Text,
    reconContextBapUri :: Maybe Text,
    reconContextBppId :: Maybe Text,
    reconContextBppUri :: Maybe Text,
    reconContextTransactionId :: Maybe Text,
    reconContextMessageId :: Maybe Text,
    reconContextTimestamp :: Maybe Text,
    reconContextTtl :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReconContext where
  parseJSON = withObject "ReconContext" $ \o ->
    ReconContext
      <$> o .:? "domain"
      <*> o .:? "country"
      <*> (o .:? "city" >>= traverse parseCityCode)
      <*> o .:? "action"
      <*> o .:? "core_version"
      <*> o .:? "bap_id"
      <*> o .:? "bap_uri"
      <*> o .:? "bpp_id"
      <*> o .:? "bpp_uri"
      <*> o .:? "transaction_id"
      <*> o .:? "message_id"
      <*> o .:? "timestamp"
      <*> o .:? "ttl"

parseCityCode :: Value -> Parser Text
parseCityCode = \case
  String s -> pure s
  Object o -> fromMaybe "" <$> o .:? "code"
  v -> typeMismatch "city (string or {code})" v

data Price = Price
  { priceCurrency :: Maybe Text,
    priceValue :: Maybe FlexAmount
  }
  deriving (Show, Eq, Generic)

instance FromJSON Price where
  parseJSON = withObject "Price" $ \o ->
    Price <$> o .:? "currency" <*> o .:? "value"

data ReceiverReconReq = ReceiverReconReq
  { receiverReconReqContext :: ReconContext,
    receiverReconReqMessage :: ReceiverReconMessage
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReceiverReconReq where
  parseJSON = withObject "ReceiverReconReq" $ \o ->
    ReceiverReconReq <$> o .: "context" <*> o .: "message"

newtype ReceiverReconMessage = ReceiverReconMessage
  { receiverReconMessageOrderbook :: Orderbook
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReceiverReconMessage where
  parseJSON = withObject "ReceiverReconMessage" $ \o ->
    ReceiverReconMessage <$> o .: "orderbook"

newtype Orderbook = Orderbook
  { orderbookOrders :: [ReconOrder]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Orderbook where
  parseJSON = withObject "Orderbook" $ \o ->
    Orderbook <$> o .: "orders"

data ReconOrder = ReconOrder
  { reconOrderId :: Maybe Text,
    reconOrderPayment :: Maybe ReconPayment,
    reconOrderSettlementId :: Maybe Text,
    reconOrderSettlementReferenceNo :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReconOrder where
  parseJSON = withObject "ReconOrder" $ \o ->
    ReconOrder
      <$> o .:? "id"
      <*> o .:? "payment"
      <*> o .:? "settlement_id"
      <*> o .:? "settlement_reference_no"

data ReconPayment = ReconPayment
  { reconPaymentParams :: Maybe ReconPaymentParams,
    reconPaymentSettlementDetails :: Maybe [SettlementDetail]
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReconPayment where
  parseJSON = withObject "ReconPayment" $ \o ->
    ReconPayment <$> o .:? "params" <*> o .:? "@ondc/org/settlement_details"

data ReconPaymentParams = ReconPaymentParams
  { reconPaymentParamsTransactionId :: Maybe Text,
    reconPaymentParamsAmount :: Maybe FlexAmount
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReconPaymentParams where
  parseJSON = withObject "ReconPaymentParams" $ \o ->
    ReconPaymentParams <$> o .:? "transaction_id" <*> o .:? "amount"

data SettlementDetail = SettlementDetail
  { settlementDetailSettlementAmount :: Maybe FlexAmount,
    settlementDetailSettlementReferenceNo :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SettlementDetail where
  parseJSON = withObject "SettlementDetail" $ \o ->
    SettlementDetail <$> o .:? "settlement_amount" <*> o .:? "settlement_reference_no"

data OnSettleReq = OnSettleReq
  { onSettleReqContext :: ReconContext,
    onSettleReqMessage :: Maybe OnSettleMessage
  }
  deriving (Show, Eq, Generic)

instance FromJSON OnSettleReq where
  parseJSON = withObject "OnSettleReq" $ \o ->
    OnSettleReq <$> o .: "context" <*> o .:? "message"

data OnSettleMessage = OnSettleMessage
  { onSettleMessageSettlementId :: Maybe Text,
    onSettleMessageStatus :: Maybe Text,
    onSettleMessageStatusReason :: Maybe Text,
    onSettleMessageOrders :: Maybe [SettlementOrderStatus],
    onSettleMessageTimestamp :: Maybe Text,
    onSettleMessageCollectorAppId :: Maybe Text,
    onSettleMessageReceiverAppId :: Maybe Text,
    onSettleMessageSettlement :: Maybe Settlement
  }
  deriving (Show, Eq, Generic)

instance FromJSON OnSettleMessage where
  parseJSON = withObject "OnSettleMessage" $ \o ->
    OnSettleMessage
      <$> o .:? "settlement_id"
      <*> o .:? "status"
      <*> o .:? "status_reason"
      <*> o .:? "orders"
      <*> o .:? "timestamp"
      <*> o .:? "collector_app_id"
      <*> o .:? "receiver_app_id"
      <*> o .:? "settlement"

data Settlement = Settlement
  { settlementType :: Maybe Text,
    settlementId :: Maybe Text,
    settlementOrders :: Maybe [SettlementOrder]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Settlement where
  parseJSON = withObject "Settlement" $ \o ->
    Settlement <$> o .:? "type" <*> o .:? "id" <*> o .:? "orders"

data SettlementOrder = SettlementOrder
  { settlementOrderId :: Maybe Text,
    settlementOrderInterParticipant :: Maybe ParticipantAmount,
    settlementOrderCollector :: Maybe ParticipantAmount,
    settlementOrderSelf :: Maybe ParticipantAmount
  }
  deriving (Show, Eq, Generic)

instance FromJSON SettlementOrder where
  parseJSON = withObject "SettlementOrder" $ \o ->
    SettlementOrder
      <$> o .:? "id"
      <*> o .:? "inter_participant"
      <*> o .:? "collector"
      <*> o .:? "self"

data ParticipantAmount = ParticipantAmount
  { participantAmountSettledAmount :: Maybe Price,
    participantAmountAmount :: Maybe Price,
    participantAmountStatus :: Maybe Text,
    participantAmountReferenceNo :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ParticipantAmount where
  parseJSON = withObject "ParticipantAmount" $ \o ->
    ParticipantAmount
      <$> o .:? "settled_amount"
      <*> o .:? "amount"
      <*> o .:? "status"
      <*> o .:? "reference_no"

data SettlementOrderStatus = SettlementOrderStatus
  { settlementOrderStatusId :: Maybe Text,
    settlementOrderStatusStatus :: Maybe Text,
    settlementOrderStatusSettlementAmount :: Maybe Price,
    settlementOrderStatusSettlementRef :: Maybe Text,
    settlementOrderStatusSettlementDate :: Maybe Text,
    settlementOrderStatusFailureReason :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SettlementOrderStatus where
  parseJSON = withObject "SettlementOrderStatus" $ \o ->
    SettlementOrderStatus
      <$> o .:? "id"
      <*> o .:? "status"
      <*> o .:? "settlement_amount"
      <*> o .:? "settlement_ref"
      <*> o .:? "settlement_date"
      <*> o .:? "failure_reason"

newtype AckResponse = AckResponse
  { ackResponseMessage :: AckMessage
  }
  deriving (Show, Eq, Generic)

instance ToJSON AckResponse where
  toJSON (AckResponse m) = object ["message" .= m]

instance FromJSON AckResponse where
  parseJSON = withObject "AckResponse" $ \o -> AckResponse <$> o .: "message"

newtype AckMessage = AckMessage {ackMessageAck :: Ack}
  deriving (Show, Eq, Generic)

instance ToJSON AckMessage where
  toJSON (AckMessage a) = object ["ack" .= a]

instance FromJSON AckMessage where
  parseJSON = withObject "AckMessage" $ \o -> AckMessage <$> o .: "ack"

newtype Ack = Ack {ackStatus :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON Ack where
  toJSON (Ack s) = object ["status" .= s]

instance FromJSON Ack where
  parseJSON = withObject "Ack" $ \o -> Ack <$> o .: "status"

ack :: AckResponse
ack = AckResponse (AckMessage (Ack "ACK"))

data OnReceiverReconReq = OnReceiverReconReq
  { onReceiverReconReqContext :: ReconContextOut,
    onReceiverReconReqMessage :: OnReceiverReconMessage
  }
  deriving (Show, Eq, Generic)

instance ToJSON OnReceiverReconReq where
  toJSON r =
    object
      [ "context" .= r.onReceiverReconReqContext,
        "message" .= r.onReceiverReconReqMessage
      ]

newtype OnReceiverReconMessage = OnReceiverReconMessage
  { onReceiverReconMessageOrderbook :: OrderbookOut
  }
  deriving (Show, Eq, Generic)

instance ToJSON OnReceiverReconMessage where
  toJSON m = object ["orderbook" .= m.onReceiverReconMessageOrderbook]

newtype OrderbookOut = OrderbookOut {orderbookOutOrders :: [RsfOrderOut]}
  deriving (Show, Eq, Generic)

instance ToJSON OrderbookOut where
  toJSON o = object ["orders" .= o.orderbookOutOrders]

data ReconContextOut = ReconContextOut
  { reconContextOutDomain :: Text,
    reconContextOutCountry :: Text,
    reconContextOutCity :: Text,
    reconContextOutAction :: Text,
    reconContextOutCoreVersion :: Text,
    reconContextOutBapId :: Text,
    reconContextOutBapUri :: Text,
    reconContextOutBppId :: Text,
    reconContextOutBppUri :: Text,
    reconContextOutTransactionId :: Text,
    reconContextOutMessageId :: Text,
    reconContextOutTimestamp :: Text,
    reconContextOutTtl :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReconContextOut where
  toJSON c =
    object
      [ "domain" .= c.reconContextOutDomain,
        "country" .= c.reconContextOutCountry,
        "city" .= c.reconContextOutCity,
        "action" .= c.reconContextOutAction,
        "core_version" .= c.reconContextOutCoreVersion,
        "bap_id" .= c.reconContextOutBapId,
        "bap_uri" .= c.reconContextOutBapUri,
        "bpp_id" .= c.reconContextOutBppId,
        "bpp_uri" .= c.reconContextOutBppUri,
        "transaction_id" .= c.reconContextOutTransactionId,
        "message_id" .= c.reconContextOutMessageId,
        "timestamp" .= c.reconContextOutTimestamp,
        "ttl" .= c.reconContextOutTtl
      ]

data RsfOrderOut = RsfOrderOut
  { rsfOrderOutId :: Text,
    rsfOrderOutCollectorAppId :: Text,
    rsfOrderOutReceiverAppId :: Text,
    rsfOrderOutTransactionId :: Maybe Text,
    rsfOrderOutSettlementId :: Maybe Text,
    rsfOrderOutSettlementReferenceNo :: Maybe Text,
    rsfOrderOutOrderReconStatus :: Text,
    rsfOrderOutCounterpartyReconStatus :: Text,
    rsfOrderOutMessage :: Maybe ReconMessageObj,
    rsfOrderOutCounterpartyDiffAmount :: NtsPrice
  }
  deriving (Show, Eq, Generic)

instance ToJSON RsfOrderOut where
  toJSON o =
    object $
      [ "id" .= o.rsfOrderOutId,
        "collector_app_id" .= o.rsfOrderOutCollectorAppId,
        "receiver_app_id" .= o.rsfOrderOutReceiverAppId,
        "order_recon_status" .= o.rsfOrderOutOrderReconStatus,
        "counterparty_recon_status" .= o.rsfOrderOutCounterpartyReconStatus,
        "counterparty_diff_amount" .= o.rsfOrderOutCounterpartyDiffAmount
      ]
        <> catMaybes
          [ ("transaction_id" .=) <$> o.rsfOrderOutTransactionId,
            ("settlement_id" .=) <$> o.rsfOrderOutSettlementId,
            ("settlement_reference_no" .=) <$> o.rsfOrderOutSettlementReferenceNo,
            ("message" .=) <$> o.rsfOrderOutMessage
          ]

data ReconMessageObj = ReconMessageObj
  { reconMessageObjName :: Text,
    reconMessageObjCode :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReconMessageObj where
  toJSON m = object ["name" .= m.reconMessageObjName, "code" .= m.reconMessageObjCode]

data NtsPrice = NtsPrice {ntsPriceCurrency :: Text, ntsPriceValue :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON NtsPrice where
  toJSON p = object ["currency" .= p.ntsPriceCurrency, "value" .= p.ntsPriceValue]

orderReconStatusFinal :: Text
orderReconStatusFinal = "02"

reconMessageFor :: Text -> Maybe ReconMessageObj
reconMessageFor = \case
  "02" -> Just (ReconMessageObj "higher amount" "high amount")
  "03" -> Just (ReconMessageObj "lesser amount" "less amount")
  _ -> Nothing
