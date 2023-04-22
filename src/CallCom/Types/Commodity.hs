{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CallCom.Types.Commodity
  ( Commodity (Commodity),
    CommodityDescription (CommodityDescription),
    CommodityId (CommodityId)
  ) where


import CallCom.JSON (base64Parser, base64ToJSON)
import CallCom.Types.CommodityType (CommodityTypeId)
import CallCom.Types.User (UserId)
import Codec.Serialise (Serialise)
import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON))
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.FromField (FromField (fromField), fromJSONField)
import Database.PostgreSQL.Simple.ToField (ToField (toField), toJSONField)
import GHC.Generics (Generic)


data Commodity =
  Commodity
    { id :: CommodityId,
      types :: Set CommodityTypeId,
      description :: Maybe CommodityDescription,
      created :: UTCTime,
      owner :: UserId
    }
  deriving (Eq, Ord, Generic, Show)

instance Serialise Commodity


newtype CommodityDescription =
  CommodityDescription
    { unCommodityDescription :: Map Text Text }
  deriving (Eq, Ord, Generic, Show, Serialise)


newtype CommodityId =
  CommodityId
    { unCommodityId :: ByteString }
  deriving (Eq, Ord, Generic, Show, Serialise)

instance ToJSON CommodityId where
  toJSON = base64ToJSON . unCommodityId

instance FromJSON CommodityId where
  parseJSON = fmap CommodityId . base64Parser

instance ToField CommodityId where
  toField = toJSONField

instance FromField CommodityId where
  fromField = fromJSONField
