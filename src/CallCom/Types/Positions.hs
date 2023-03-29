{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}


module CallCom.Types.Positions
  ( Positions (Positions)
  ) where


import CallCom.Types.Commodity (Commodity)
import CallCom.Types.CommodityType (CommodityTypeId)
import CallCom.Types.TokenBalance (TokenBalance)
import CallCom.Types.TokenIssue (TokenIssueId)
import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import GHC.Generics (Generic)


data Positions =
  Positions
    { spot :: Map CommodityTypeId (Set Commodity),
      debits :: Map TokenIssueId TokenBalance, -- value owed to the position holder
      credits :: Map TokenIssueId TokenBalance -- value owed by the position holder
    }
  deriving Generic

instance Semigroup Positions where
  p <> q = Positions
            (Map.unionWith (<>) (p ^. #spot) (q ^. #spot))
            (Map.unionWith (+) (p ^. #debits) (q ^. #debits))
            (Map.unionWith (+) (p ^. #credits) (q ^. #credits))

instance Monoid Positions where
  mempty = Positions mempty mempty mempty
