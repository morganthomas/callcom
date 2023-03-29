{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.Auth
  ( LoginPassword (LoginPassword),
    LoginPasswordHash (LoginPasswordHash),
    SpendingPassword (SpendingPassword),
    SpendingPasswordHash (SpendingPasswordHash),
    Signature (Signature),
    UserPublicKey (UserPublicKey),
    UserPrivateKey (UserPrivateKey)
  ) where


import Crypto.Sign.Ed25519 (PublicKey, SecretKey, Signature (Signature))
import Data.ByteString (ByteString)
import GHC.Generics (Generic)


newtype LoginPassword =
  LoginPassword { unLoginPassword :: ByteString }
  deriving Generic

newtype LoginPasswordHash =
  LoginPasswordHash { unLoginPasswordHash :: ByteString }
  deriving Generic

newtype SpendingPassword =
  SpendingPassword { unSpendingPassword :: ByteString }
  deriving Generic

newtype SpendingPasswordHash =
  SpendingPasswordHash { unSpendingPasswordHash :: ByteString }
  deriving Generic

newtype UserPublicKey =
  UserPublicKey { unUserPublicKey :: PublicKey }
  deriving Generic

newtype UserPrivateKey =
  UserPrivateKey { unUserPrivateKey :: SecretKey }
  deriving Generic
