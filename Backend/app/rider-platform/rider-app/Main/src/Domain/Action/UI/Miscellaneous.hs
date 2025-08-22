module Domain.Action.UI.Miscellaneous (postMiscTestScanQR) where

import qualified API.Types.UI.Miscellaneous
import qualified Domain.Action.Beckn.FRFS.OnConfirm as OC
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (error, id)
import qualified Kernel.Prelude hiding (error)
import qualified Kernel.Types.Id

postMiscTestScanQR ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.Miscellaneous.QRScanTestReq ->
    Environment.Flow API.Types.UI.Miscellaneous.QRScanTestResp
  )
postMiscTestScanQR _ req = do
  res <- liftIO $ OC.scanQRFromBase64 req.base64Image
  pure . uncurry API.Types.UI.Miscellaneous.QRScanTestResp $ case res of
    Right res' -> (Nothing, Just res')
    Left err -> (Just err, Nothing)
