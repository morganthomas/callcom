{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CallCom.Types.User
  ( User (User),
    UserId (UserId),
    UserName (UserName)
  ) where


import CallCom.Types.Auth (UserPublicKey)
import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)


data User =
  User
   { id :: UserId,
     name :: UserName,
     referrer :: Maybe UserId,
     created :: UTCTime,
     pubkey :: UserPublicKey
   }
  deriving (Eq, Generic, Show)

instance Serialise User


newtype UserId =
  UserId
    { unUserId :: ByteString }
  deriving (Eq, Ord, Generic, Serialise, Show)


newtype UserName =
  UserName
    { unUserName :: Text }
  deriving (Eq, Ord, Generic, Serialise, Show)
