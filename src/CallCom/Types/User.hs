{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.User
  ( User (User),
    UserId (UserId),
    UserName (UserName)
  ) where


import Data.Text (Text)
import GHC.Generics (Generic)


data User =
  User
   { id :: UserId,
     name :: UserName
   }
  deriving Generic


newtype UserId =
  UserId
    { unUserId :: Int }
  deriving Generic


newtype UserName =
  UserName
    { unUserName :: Text }
  deriving Generic
