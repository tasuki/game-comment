module Utils where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

lazyTextToByteString :: TL.Text -> BS.ByteString
lazyTextToByteString = LBS.toStrict . TLE.encodeUtf8

byteStringToLazyText :: BS.ByteString -> TL.Text
byteStringToLazyText = TL.fromStrict . TE.decodeUtf8

byteStringToString :: BS.ByteString -> String
byteStringToString = BS.unpack

lbsToLazyText :: LBS.ByteString -> TL.Text
lbsToLazyText = TLE.decodeUtf8

lazyTextToLbs :: TL.Text -> LBS.ByteString
lazyTextToLbs = TLE.encodeUtf8

lazyTextToText :: TL.Text -> T.Text
lazyTextToText = TL.toStrict

stringToInt :: String -> Maybe Int
stringToInt str = case reads str of
    [(x, "")] -> Just x
    _         -> Nothing

stringToLazyText :: String -> TL.Text
stringToLazyText = TL.fromStrict . T.pack

stringToText :: String -> T.Text
stringToText = T.pack

stringToByteString :: String -> BS.ByteString
stringToByteString = BS.pack
