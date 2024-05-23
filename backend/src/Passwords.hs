{-# LANGUAGE OverloadedStrings #-}

module Passwords (hashPassword, verifyPassword) where

import Crypto.Scrypt
import qualified Data.Text.Lazy as TL

import Utils

salt :: Salt
salt = Salt "SaltyPeanutIsSalty"

toPass :: TL.Text -> Pass
toPass = Pass . lazyTextToByteString

hashPassword :: TL.Text -> TL.Text
hashPassword plainTextPass = byteStringToLazyText $ getEncryptedPass
    $ encryptPass defaultParams salt (toPass plainTextPass)

verifyPassword :: TL.Text -> TL.Text -> Bool
verifyPassword hashedPassword plainTextPass =
    case verifyPass defaultParams (toPass plainTextPass) encryptedPass of
        (isValid, _) -> isValid
    where encryptedPass = EncryptedPass $ lazyTextToByteString hashedPassword
