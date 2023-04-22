{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}


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


import CallCom.JSON (base64Parser)
import Codec.Serialise (Serialise)
import Crypto.Sign.Ed25519 (PublicKey (PublicKey), SecretKey (SecretKey), Signature (Signature))
import Data.Aeson (FromJSON (parseJSON))
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple.FromField (FromField)
import GHC.Generics (Generic)


instance Serialise PublicKey

deriving instance FromField PublicKey

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

instance FromJSON PublicKey where
  parseJSON = fmap PublicKey . base64Parser

newtype UserPublicKey =
  UserPublicKey { unUserPublicKey :: PublicKey }
  deriving (Eq, Generic, Show, Serialise, FromField, FromJSON)

newtype UserPrivateKey =
  UserPrivateKey { unUserPrivateKey :: SecretKey }
  deriving (Eq, Generic, Show)
