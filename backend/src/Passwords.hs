{-# LANGUAGE OverloadedStrings #-}

module Passwords (hashPassword, verifyPassword) where

import qualified Crypto.Scrypt as S
import qualified Data.Text.Lazy as TL

import qualified Utils as U

toPass :: TL.Text -> S.Pass
toPass = S.Pass . U.lazyTextToByteString

hashPassword :: String -> TL.Text -> TL.Text
hashPassword salt plainTextPass = U.byteStringToLazyText $ S.getEncryptedPass
    $ S.encryptPass S.defaultParams (S.Salt (U.stringToByteString salt)) $ toPass plainTextPass

verifyPassword :: TL.Text -> TL.Text -> Bool
verifyPassword hashedPassword plainTextPass =
    case S.verifyPass S.defaultParams (toPass plainTextPass) encryptedPass of
        (isValid, _) -> isValid
    where encryptedPass = S.EncryptedPass $ U.lazyTextToByteString hashedPassword
