module Utils (lazyTextToByteString, byteStringToLazyText, lbsToLazyText) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

lazyTextToByteString :: TL.Text -> BS.ByteString
lazyTextToByteString = LBS.toStrict . TLE.encodeUtf8

byteStringToLazyText :: BS.ByteString -> TL.Text
byteStringToLazyText = TL.fromStrict . TE.decodeUtf8

lbsToLazyText :: LBS.ByteString -> TL.Text
lbsToLazyText = TLE.decodeUtf8
