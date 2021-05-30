{-# LANGUAGE GADTs, FlexibleContexts, DataKinds #-}

module Freer where

import Control.Monad.Freer


data Console r where
  GetLine :: Console String
  PutStrLn :: String -> Console ()
