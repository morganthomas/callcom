{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CallCom.Types.User
  ( User (User),
    UserId (UserId),
    UserName (UserName)
  ) where


import CallCom.JSON (base64Parser)
import CallCom.Types.Auth (UserPublicKey, Signature)
import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON (parseJSON))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.Generics (Generic)


data User =
  User
   { id :: UserId,
     name :: UserName,
     referrer :: Maybe UserId,
     referrerSignature :: Maybe Signature, -- should be present unless user is the initial user
     created :: UTCTime,
     pubkey :: UserPublicKey
   }
  deriving (Eq, Generic, Show)

instance Serialise User


newtype UserId =
  UserId
    { unUserId :: ByteString }
  deriving (Eq, Ord, Generic, Serialise, Show, FromField, ToField)

instance FromJSON UserId where
  parseJSON x = UserId <$> base64Parser x


newtype UserName =
  UserName
    { unUserName :: Text }
  deriving (Eq, Ord, Generic, Serialise, Show, FromField)
