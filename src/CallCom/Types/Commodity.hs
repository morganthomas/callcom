{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.Commodity
  ( Commodity (Commodity),
    CommodityDescription (CommodityDescription),
    CommodityId (CommodityId)
  ) where


import CallCom.Types.CommodityType (CommodityTypeId)
import CallCom.Types.User (UserId)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)


data Commodity =
  Commodity
    { types :: Set CommodityTypeId,
      id :: CommodityId,
      description :: Maybe CommodityDescription,
      owner :: UserId
    }
  deriving (Generic)


newtype CommodityDescription =
  CommodityDescription
    { unCommodityDescription :: Text }
  deriving Generic


newtype CommodityId =
  CommodityId
    { unCommodityId :: Int }
  deriving Generic
