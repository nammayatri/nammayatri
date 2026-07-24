{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Fetch a file from an arbitrary third-party URL so it can be rehosted on our
-- own S3. Used by the Xyne webhook, which receives attachments as links rather
-- than bytes.
--
-- The url comes from an inbound payload, so this is a server-side fetch of a
-- caller-influenced address: the reads are bounded and the destination is
-- screened before connecting.
module IssueManagement.Utils.RemoteFile
  ( RemoteFile (..),
    fetchRemoteFile,
  )
where

import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word16, Word8)
import Kernel.Prelude
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as HCT
import qualified Network.Socket as NS

data RemoteFile = RemoteFile
  { content :: BS.ByteString,
    -- | @Content-Type@ with any @;charset=…@ parameters stripped. Empty when the
    -- server did not send one.
    contentType :: Text
  }

-- | How long a single fetch may take before it is abandoned. Without this the
-- manager default applies and a slow endpoint can park a thread indefinitely.
fetchTimeoutSeconds :: Int
fetchTimeoutSeconds = 30

-- | GET the url and return its bytes, or 'Nothing' if the body exceeds
-- @maxBytes@.
--
-- Built on 'HC.parseUrlThrow', so a non-2xx response raises an exception rather
-- than yielding a body of error HTML. Connection failures, blocked destinations
-- and timeouts throw too — callers are expected to run this inside a @try@ and
-- fall back to leaving the original url in place, so a fetch failure degrades
-- rather than losing the attachment.
--
-- The body is read incrementally and abandoned as soon as it exceeds
-- @maxBytes@, so an oversized (or endless) response cannot be buffered into
-- memory before being rejected.
fetchRemoteFile :: MonadIO m => Text -> Int -> m (Maybe RemoteFile)
fetchRemoteFile url maxBytes = liftIO $ do
  parsed <- HC.parseUrlThrow (T.unpack url)
  let req =
        parsed
          { HC.responseTimeout = HC.responseTimeoutMicro (fetchTimeoutSeconds * 1000000)
          }
  assertDestinationAllowed (HC.host req) (HC.port req)
  manager <- HCT.newTlsManager
  HC.withResponse req manager $ \resp -> do
    mbBody <- readAtMost (HC.responseBody resp) maxBytes
    pure $ (\body -> RemoteFile body (responseContentType resp)) <$> mbBody

-- | Read a streaming response body, giving up as soon as @limit@ is exceeded.
readAtMost :: HC.BodyReader -> Int -> IO (Maybe BS.ByteString)
readAtMost bodyReader limit = go 0 []
  where
    go acc chunks = do
      chunk <- HC.brRead bodyReader
      if BS.null chunk
        then pure . Just . BS.concat $ reverse chunks
        else do
          let acc' = acc + BS.length chunk
          if acc' > limit
            then pure Nothing
            else go acc' (chunk : chunks)

-- | Refuse to fetch anything that resolves to a loopback, private or otherwise
-- internal address.
--
-- The url is attacker-influenced, so without this the webhook becomes an SSRF
-- primitive: a request for @http://169.254.169.254/…@ or an internal service
-- would be issued with our network position. Resolution happens here rather
-- than matching on the literal host, because a public hostname can resolve to a
-- private address.
--
-- Note this does not close the gap entirely — the connection re-resolves, so a
-- name that changes answers between this check and the connect (DNS rebinding)
-- can still slip through. Closing that needs a custom connection hook.
assertDestinationAllowed :: BS.ByteString -> Int -> IO ()
assertDestinationAllowed hostBs port = do
  let host = T.unpack $ TE.decodeUtf8With (\_ _ -> Just '?') hostBs
      hints = NS.defaultHints {NS.addrSocketType = NS.Stream}
  addrs <- NS.getAddrInfo (Just hints) (Just host) (Just (show port))
  when (any (isInternal . NS.addrAddress) addrs) $
    ioError . userError $ "refusing to fetch internal address for host " <> host

isInternal :: NS.SockAddr -> Bool
isInternal = \case
  NS.SockAddrInet _ ha -> isInternalV4 (NS.hostAddressToTuple ha)
  NS.SockAddrInet6 _ _ ha6 _ -> isInternalV6 (NS.hostAddress6ToTuple ha6)
  -- Unix-domain and anything unrecognised: nothing legitimate for us to fetch.
  _ -> True

isInternalV4 :: (Word8, Word8, Word8, Word8) -> Bool
isInternalV4 (a, b, _, _) =
  a == 0 -- 0.0.0.0/8 "this network"
    || a == 10 -- private
    || a == 127 -- loopback
    || (a == 100 && b >= 64 && b <= 127) -- carrier-grade NAT
    || (a == 169 && b == 254) -- link-local, incl. cloud metadata
    || (a == 172 && b >= 16 && b <= 31) -- private
    || (a == 192 && b == 168) -- private
    || a >= 224 -- multicast and reserved

isInternalV6 :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16) -> Bool
isInternalV6 (a, b, c, d, e, f, g, h) =
  allZero && (h == 0 || h == 1) -- :: and ::1
    || (a .&. 0xfe00) == 0xfc00 -- unique local fc00::/7
    || (a .&. 0xffc0) == 0xfe80 -- link-local fe80::/10
  where
    allZero = all (== 0) [a, b, c, d, e, f, g]

responseContentType :: HC.Response a -> Text
responseContentType resp =
  case lookup "Content-Type" (HC.responseHeaders resp) of
    Nothing -> ""
    Just bs -> T.strip . T.takeWhile (/= ';') $ TE.decodeUtf8With (\_ _ -> Just '?') bs
