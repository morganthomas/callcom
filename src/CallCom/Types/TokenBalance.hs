{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CallCom.Types.TokenBalance
  ( TokenBalance (TokenBalance)
  ) where


import GHC.Generics (Generic)


newtype TokenBalance =
  TokenBalance
    { unTokenBalance :: Integer }
  deriving (Eq, Ord, Generic, Num, Show)
