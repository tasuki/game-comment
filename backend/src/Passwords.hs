module Passwords (hashPassword, verifyPassword) where

import qualified Crypto.Scrypt as S
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

toPass :: TL.Text -> S.Pass
toPass = S.Pass . LBS.toStrict . TLE.encodeUtf8

hashPassword :: String -> TL.Text -> TL.Text
hashPassword salt = TLE.decodeUtf8 . LBS.fromStrict . S.getEncryptedPass
    . S.encryptPass S.defaultParams (S.Salt (BS.pack salt)) . toPass

verifyPassword :: TL.Text -> TL.Text -> Bool
verifyPassword hashed plain =
    fst $ S.verifyPass S.defaultParams (toPass plain)
        (S.EncryptedPass $ LBS.toStrict $ TLE.encodeUtf8 hashed)
