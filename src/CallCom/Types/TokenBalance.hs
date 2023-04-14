{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CallCom.Types.TokenBalance
  ( TokenBalance (TokenBalance),
    TokenCreditBalance (TokenCreditBalance),
    TokenDebitBalance (TokenDebitBalance)
  ) where


import Codec.Serialise (Serialise)
import GHC.Generics (Generic)


newtype TokenBalance =
  TokenBalance
    { unTokenBalance :: Integer }
  deriving (Eq, Ord, Generic, Num, Show, Serialise)


newtype TokenCreditBalance =
  TokenCreditBalance
    { unTokenCreditBalance :: TokenBalance }
  deriving (Eq, Ord, Generic, Num, Show, Serialise)


newtype TokenDebitBalance =
  TokenDebitBalance
    { unTokenDebitBalance :: TokenBalance }
  deriving (Eq, Ord, Generic, Num, Show, Serialise)
