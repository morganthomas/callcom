{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}


module CallCom.Types.Positions
  ( Positions (Positions),
    subtractPositions,
    removeZeroPositions
  ) where


import CallCom.Types.Commodity (Commodity)
import CallCom.Types.TokenBalance (TokenBalance)
import CallCom.Types.TokenIssue (TokenIssueId)
import Codec.Serialise (Serialise)
import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)


data Positions =
  Positions
    { spot :: Set Commodity,
      debits :: Map TokenIssueId TokenBalance, -- value owed to the position holder
      credits :: Map TokenIssueId TokenBalance -- value owed by the position holder
    }
  deriving (Eq, Generic, Show)

instance Serialise Positions

instance Semigroup Positions where
  p <> q = Positions
            ((p ^. #spot) <> (q ^. #spot))
            (Map.unionWith (+) (p ^. #debits) (q ^. #debits))
            (Map.unionWith (+) (p ^. #credits) (q ^. #credits))


instance Monoid Positions where
  mempty = Positions mempty mempty mempty


subtractPositions :: Positions -> Positions -> Positions
subtractPositions p q =
  removeZeroPositions $
  Positions
    ((p ^. #spot) `Set.difference` (q ^. #spot))
    (Map.unionWith (+) (p ^. #debits) (negate <$> (q ^. #debits)))
    (Map.unionWith (+) (p ^. #credits) (negate <$> (q ^. #credits)))


removeZeroPositions :: Positions -> Positions
removeZeroPositions p =
  Positions
    (p ^. #spot)
    (Map.filter (/= 0) (p ^. #debits))
    (Map.filter (/= 0) (p ^. #credits))
