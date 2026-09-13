module BecknV2.NTS10.APIs where

import qualified BecknV2.NTS10.Types as Spec
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type ReceiverReconAPI =
  "receiver_recon"
    :> ReqBody '[JSON] Spec.ReceiverReconReq
    :> Post '[JSON] Spec.AckResponse

receiverReconAPI :: Proxy ReceiverReconAPI
receiverReconAPI = Proxy

type OnSettleAPI =
  "on_settle"
    :> ReqBody '[JSON] Spec.OnSettleReq
    :> Post '[JSON] Spec.AckResponse

onSettleAPI :: Proxy OnSettleAPI
onSettleAPI = Proxy

type OnReceiverReconAPI =
  "on_receiver_recon"
    :> ReqBody '[JSON] Spec.OnReceiverReconReq
    :> Post '[JSON] Spec.AckResponse

onReceiverReconAPI :: Proxy OnReceiverReconAPI
onReceiverReconAPI = Proxy
