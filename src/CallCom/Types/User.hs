{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.User
  ( User (User),
    UserId (UserId),
    UserName (UserName),
    UserEmailAddress (UserEmailAddress)
  ) where


import CallCom.Types.Auth (UserPublicKey)
import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)


data User =
  User
   { id :: UserId,
     email :: UserEmailAddress,
     name :: UserName,
     referrers :: Set UserId,
     created :: UTCTime,
     pubkey :: UserPublicKey
   }
  deriving Generic


newtype UserId =
  UserId
    { unUserId :: ByteString }
  deriving (Eq, Ord, Generic)


newtype UserName =
  UserName
    { unUserName :: Text }
  deriving (Eq, Ord, Generic)


newtype UserEmailAddress =
  UserEmailAddress
    { unUserEmailAddress :: ByteString }
  deriving (Eq, Ord, Generic)
