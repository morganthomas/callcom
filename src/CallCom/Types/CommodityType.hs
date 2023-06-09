{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CallCom.Types.CommodityType
  ( CommodityType (CommodityType),
    CommodityTypeId (CommodityTypeId),
    CommodityTypeName (CommodityTypeName)
  ) where


import CallCom.Types.User (UserId)
import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.Generics (Generic)


data CommodityType =
  CommodityType
    { name :: CommodityTypeName,
      created :: UTCTime,
      author :: UserId
    }
  deriving (Generic, Show)

instance Serialise CommodityType


newtype CommodityTypeId =
  CommodityTypeId
    { unCommodityTypeId :: ByteString }
  deriving (Eq, Ord, Generic, Show, Serialise, FromField, ToField)


newtype CommodityTypeName =
  CommodityTypeName
    { unCommodityTypeName :: Text }
  deriving (Eq, Ord, Generic, Show, Serialise, FromField)
