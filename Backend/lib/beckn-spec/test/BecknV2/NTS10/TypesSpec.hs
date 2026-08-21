module BecknV2.NTS10.TypesSpec (spec) where

import qualified BecknV2.NTS10.Types as Spec
import Data.Aeson (decode, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import Kernel.Prelude
import Test.Hspec

spec :: Spec
spec = describe "BecknV2.NTS10.Types" $ do
  describe "FlexAmount" $ do
    it "accepts the string form the spec defines" $
      decode "\"8.50\"" `shouldBe` Just (Spec.FlexAmount "8.50")

    it "accepts the bare number form the high-volume BAPs actually send" $
      decode "8.5" `shouldBe` Just (Spec.FlexAmount "8.5")

    it "preserves the string form's trailing zero rather than renormalising it" $
      (Spec.getFlexAmount <$> decode "\"8.50\"") `shouldBe` Just "8.50"

    it "renders a decimal number without scientific notation" $
      (Spec.getFlexAmount <$> decode "8.5") `shouldBe` Just "8.5"

    it "treats an integral number as an integer, not 8.0" $
      (Spec.getFlexAmount <$> decode "8") `shouldBe` Just "8"

    it "reads null as empty rather than failing the whole request" $
      decode "null" `shouldBe` Just (Spec.FlexAmount "")

    it "still REJECTS a non-numeric, non-string token" $
      (eitherDecode "true" :: Either String Spec.FlexAmount) `shouldSatisfy` (\case Left _ -> True; Right _ -> False)

    it "marshals back out as the spec-compliant string form" $
      encode (Spec.FlexAmount "8.5") `shouldBe` "\"8.5\""

    it "converts to a number for arithmetic, from either wire form" $ do
      (Spec.flexAmountToDouble =<< decode "8.5") `shouldBe` Just 8.5
      (Spec.flexAmountToDouble =<< decode "\"8.50\"") `shouldBe` Just 8.5

    it "gives Nothing for an unreadable amount rather than defaulting to zero" $
      Spec.flexAmountToDouble (Spec.FlexAmount "") `shouldBe` Nothing

  describe "ReconContext.city" $ do
    it "accepts the bare string form" $
      (Spec.reconContextCity <$> decode "{\"city\":\"std:044\"}") `shouldBe` Just (Just "std:044")

    it "accepts the beckn object form" $
      (Spec.reconContextCity <$> decode "{\"city\":{\"code\":\"std:044\"}}") `shouldBe` Just (Just "std:044")

    it "reads core_version, which RSF calls that and TRV11 calls version" $
      (Spec.reconContextCoreVersion <$> decode "{\"core_version\":\"2.0.0\"}") `shouldBe` Just (Just "2.0.0")

  describe "receiver_recon" $
    it "reads the settlement amount out of the @ondc/org/ key, in both forms" $ do
      let body form =
            "{\"context\":{\"domain\":\"ONDC:NTS10\"},\"message\":{\"orderbook\":{\"orders\":[{\"id\":\"CUM123\",\
            \\"payment\":{\"params\":{\"transaction_id\":\"T1\",\"amount\":"
              <> form
              <> "},\
                 \\"@ondc/org/settlement_details\":[{\"settlement_amount\":"
              <> form
              <> "}]}}]}}}"
          amountOf b = do
            req <- decode b :: Maybe Spec.ReceiverReconReq
            order <- listToMaybe . Spec.orderbookOrders . Spec.receiverReconMessageOrderbook $ Spec.receiverReconReqMessage req
            payment <- Spec.reconOrderPayment order
            details <- Spec.reconPaymentSettlementDetails payment
            detail <- listToMaybe details
            Spec.flexAmountToDouble =<< Spec.settlementDetailSettlementAmount detail
      amountOf (body "\"120.00\"") `shouldBe` Just 120.0
      amountOf (body "120") `shouldBe` Just 120.0

  describe "receiver_recon settlement reference" $ do
    let refsOf b = do
          req <- decode b :: Maybe Spec.ReceiverReconReq
          order <- listToMaybe . Spec.orderbookOrders . Spec.receiverReconMessageOrderbook $ Spec.receiverReconReqMessage req
          let onOrder = Spec.reconOrderSettlementReferenceNo order
              inDetail = do
                payment <- Spec.reconOrderPayment order
                details <- Spec.reconPaymentSettlementDetails payment
                detail <- listToMaybe details
                Spec.settlementDetailSettlementReferenceNo detail
          pure (onOrder, inDetail)

    it "reads it from the order" $
      refsOf
        "{\"context\":{},\"message\":{\"orderbook\":{\"orders\":[{\"id\":\"CUM123\",\
        \\"settlement_id\":\"S1\",\"settlement_reference_no\":\"R1\"}]}}}"
        `shouldBe` Just (Just "R1", Nothing)

    it "reads it from the settlement detail beside the amount" $
      refsOf
        "{\"context\":{},\"message\":{\"orderbook\":{\"orders\":[{\"id\":\"CUM123\",\
        \\"payment\":{\"@ondc/org/settlement_details\":[{\"settlement_amount\":120,\
        \\"settlement_reference_no\":\"R2\"}]}}]}}}"
        `shouldBe` Just (Nothing, Just "R2")

    it "yields Nothing for both when the sender omits it" $
      refsOf "{\"context\":{},\"message\":{\"orderbook\":{\"orders\":[{\"id\":\"CUM123\"}]}}}"
        `shouldBe` Just (Nothing, Nothing)

  describe "on_settle" $ do
    it "reads the current nested settlement shape" $ do
      let b =
            "{\"context\":{},\"message\":{\"settlement\":{\"id\":\"S1\",\"orders\":[{\"id\":\"CUM123\",\
            \\"self\":{\"settled_amount\":{\"currency\":\"INR\",\"value\":120},\"status\":\"SETTLED\",\"reference_no\":\"R1\"}}]}}}"
      let selfOf = do
            req <- decode b :: Maybe Spec.OnSettleReq
            msg <- Spec.onSettleReqMessage req
            s <- Spec.onSettleMessageSettlement msg
            orders <- Spec.settlementOrders s
            order <- listToMaybe orders
            Spec.settlementOrderSelf order
      (Spec.participantAmountStatus =<< selfOf) `shouldBe` Just "SETTLED"
      (Spec.participantAmountReferenceNo =<< selfOf) `shouldBe` Just "R1"

    it "also reads the legacy flat shape Go still accepts" $ do
      let b =
            "{\"context\":{},\"message\":{\"settlement_id\":\"S1\",\"status\":\"SUCCESS\",\
            \\"orders\":[{\"id\":\"CUM123\",\"status\":\"SETTLED\",\"settlement_ref\":\"R1\"}]}}"
      let msg = Spec.onSettleReqMessage =<< (decode b :: Maybe Spec.OnSettleReq)
      (Spec.onSettleMessageSettlementId =<< msg) `shouldBe` Just "S1"
      ((Spec.settlementOrderStatusStatus <=< listToMaybe) =<< (Spec.onSettleMessageOrders =<< msg))
        `shouldBe` Just "SETTLED"

  describe "ack" $
    it "answers ACK, as the Go service does, not the 200 other specs hardcode" $
      BSL.toStrict (encode Spec.ack) `shouldBe` "{\"message\":{\"ack\":{\"status\":\"ACK\"}}}"
