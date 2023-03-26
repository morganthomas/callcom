{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.CommodityType
  ( CommodityType (CommodityType),
    CommodityTypeId (CommodityTypeId),
    CommodityTypeName (CommodityTypeName)
  ) where


import CallCom.Types.User (UserId)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)


data CommodityType =
  CommodityType
    { id :: CommodityTypeId,
      name :: CommodityTypeName,
      created :: UTCTime,
      author :: UserId
    }
  deriving Generic


newtype CommodityTypeId =
  CommodityTypeId
    { unCommodityTypeId :: ByteString }
  deriving Generic


newtype CommodityTypeName =
  CommodityTypeName
    { unCommodityTypeName :: Text }
  deriving Generic
