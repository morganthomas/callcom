{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.Positions
  ( Positions (Positions)
  ) where


import CallCom.Types.Commodity (Commodity)
import CallCom.Types.CommodityType (CommodityTypeId)
import CallCom.Types.TokenBalance (TokenBalance)
import CallCom.Types.TokenIssue (TokenIssueId)
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics (Generic)


data Positions =
  Positions
    { spot :: Map CommodityTypeId (Set Commodity),
      credits :: Map TokenIssueId TokenBalance,
      debts :: Map TokenIssueId TokenBalance
    }
  deriving Generic
