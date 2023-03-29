{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.Auth
  ( LoginPassword (LoginPassword),
    LoginPasswordHash (LoginPasswordHash),
    SpendingPassword (SpendingPassword),
    SpendingPasswordHash (SpendingPasswordHash),
    Signature (Signature),
    PublicKey (PublicKey),
    SecretKey (SecretKey),
    UserPublicKey (UserPublicKey),
    UserPrivateKey (UserPrivateKey)
  ) where


import Crypto.Sign.Ed25519 (PublicKey (PublicKey), SecretKey (SecretKey), Signature (Signature))
import Data.ByteString (ByteString)
import GHC.Generics (Generic)


newtype LoginPassword =
  LoginPassword { unLoginPassword :: ByteString }
  deriving (Generic, Show)

newtype LoginPasswordHash =
  LoginPasswordHash { unLoginPasswordHash :: ByteString }
  deriving (Generic, Show)

newtype SpendingPassword =
  SpendingPassword { unSpendingPassword :: ByteString }
  deriving (Generic, Show)

newtype SpendingPasswordHash =
  SpendingPasswordHash { unSpendingPasswordHash :: ByteString }
  deriving (Generic, Show)

newtype UserPublicKey =
  UserPublicKey { unUserPublicKey :: PublicKey }
  deriving (Generic, Show)

newtype UserPrivateKey =
  UserPrivateKey { unUserPrivateKey :: SecretKey }
  deriving (Generic, Show)
