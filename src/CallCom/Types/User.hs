{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CallCom.Types.User
  ( User (User),
    UserId (UserId),
    UserName (UserName),
    UserEmailAddress (UserEmailAddress)
  ) where


import CallCom.Types.Auth (UserPublicKey)
import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)


data User =
  User
   { id :: UserId,
     name :: UserName,
     referrers :: Set UserId,
     created :: UTCTime,
     pubkey :: UserPublicKey
   }
  deriving Generic


newtype UserId =
  UserId
    { unUserId :: ByteString }
  deriving (Eq, Ord, Generic, Serialise)


newtype UserName =
  UserName
    { unUserName :: Text }
  deriving (Eq, Ord, Generic, Serialise)


newtype UserEmailAddress =
  UserEmailAddress
    { unUserEmailAddress :: ByteString }
  deriving (Eq, Ord, Generic)
