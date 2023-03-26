{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.User
  ( User (User),
    UserId (UserId),
    UserName (UserName)
  ) where


import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)


data User =
  User
   { id :: UserId,
     name :: UserName,
     referrers :: Set UserId,
     created :: UTCTime
   }
  deriving Generic


newtype UserId =
  UserId
    { unUserId :: Text }
  deriving Generic


newtype UserName =
  UserName
    { unUserName :: Text }
  deriving Generic
