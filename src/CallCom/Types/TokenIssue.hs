{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.TokenIssue
  ( TokenIssue (TokenIssue),
    TokenIssueId (TokenIssueId),
    TokenFraction (TokenFraction)
  ) where


import CallCom.Types.CommodityType (CommodityTypeId)
import CallCom.Types.TokenBalance (TokenBalance)
import CallCom.Types.User (UserId)
import Data.ByteString (ByteString)
import Data.Set (Set)
import GHC.Generics (Generic)


data TokenIssue =
  TokenIssue
    { id :: TokenIssueId,
      underlying :: CommodityTypeId,
      fraction :: TokenFraction,
      issuers :: Set UserId,
      circulation :: TokenBalance
    }
  deriving (Eq, Ord, Generic, Show)


newtype TokenIssueId =
  TokenIssueId
    { unTokenIssueId :: ByteString }
  deriving (Eq, Ord, Generic, Show)


-- The number of tokens per unit of underlying asset
newtype TokenFraction =
  TokenFraction
    { unTokenFraction :: Int }
  deriving (Eq, Ord, Generic, Show)
