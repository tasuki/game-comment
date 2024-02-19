{-# LANGUAGE OverloadedStrings #-}

module Passwords (hashPassword, verifyPassword) where

import Crypto.Scrypt
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE

salt :: Salt
salt = Salt "SaltyPeanutIsSalty"

lazyTextToByteString :: TL.Text -> BS.ByteString
lazyTextToByteString = LBS.toStrict . TLE.encodeUtf8

byteStringToLazyText :: BS.ByteString -> TL.Text
byteStringToLazyText = TL.fromStrict . TE.decodeUtf8

toPass :: TL.Text -> Pass
toPass = Pass . lazyTextToByteString

hashPassword :: TL.Text -> TL.Text
hashPassword plainTextPass = byteStringToLazyText $ getEncryptedPass
    $ encryptPass defaultParams salt (toPass plainTextPass)

verifyPassword :: TL.Text -> TL.Text -> Bool
verifyPassword plainTextPass hashedPassword =
    case verifyPass defaultParams (toPass plainTextPass) encryptedPass of
        (isValid, _) -> isValid
    where encryptedPass = EncryptedPass $ lazyTextToByteString hashedPassword