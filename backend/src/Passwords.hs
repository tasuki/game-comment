{-# LANGUAGE OverloadedStrings #-}

module Passwords (hashPassword, verifyPassword) where

import qualified Crypto.Scrypt as S
import qualified Data.Text.Lazy as TL

import qualified Utils as U

salt :: S.Salt
salt = S.Salt "SaltyPeanutIsSalty"

toPass :: TL.Text -> S.Pass
toPass = S.Pass . U.lazyTextToByteString

hashPassword :: TL.Text -> TL.Text
hashPassword plainTextPass = U.byteStringToLazyText $ S.getEncryptedPass
    $ S.encryptPass S.defaultParams salt $ toPass plainTextPass

verifyPassword :: TL.Text -> TL.Text -> Bool
verifyPassword hashedPassword plainTextPass =
    case S.verifyPass S.defaultParams (toPass plainTextPass) encryptedPass of
        (isValid, _) -> isValid
    where encryptedPass = S.EncryptedPass $ U.lazyTextToByteString hashedPassword
