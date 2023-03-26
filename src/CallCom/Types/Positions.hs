{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.Positions
  ( Positions (Positions)
  ) where


import CallCom.Types.Commodity (Commodity)
import CallCom.Types.CommodityType (CommodityType)
import CallCom.Types.TokenBalance (TokenBalance)
import CallCom.Types.TokenType (TokenType)
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics (Generic)


data Positions =
  Positions
    { holdings :: Map CommodityType (Set Commodity),
      credits :: Map TokenType TokenBalance,
      debts :: Map TokenType TokenBalance
    }
  deriving Generic
