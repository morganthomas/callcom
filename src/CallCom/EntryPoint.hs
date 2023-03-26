{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CallCom.EntryPoint
  ( main,
  )
where

main :: IO ()
main = do
  putStrLn "hello world"
