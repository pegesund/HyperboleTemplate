{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}



module Main where

import Data.Text (Text)
import Web.Hyperbole
import Effectful
import Logger (logInfo, logInfoM)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  -- Log application startup
  logInfo "Starting Hyperbole application on port 3000"
  
  run 3000 $ do
    liveApp (basicDocument "Example") (runPage mypage)


mypage :: (Hyperbole :> es) => Eff es (Page '[Message])
mypage = do
  pure $ col id $ do
    hyper Message1 $ messageView "Hello"
    hyper Message2 $ messageView "World!"


data Message = Message1 | Message2
  deriving (Show, Read, ViewId)


instance (IOE :> es) => HyperView Message es where
  data Action Message = Louder Text
    deriving (Show, Read, ViewAction)

  update (Louder msg) = do
    -- Log when the action is executed
    liftIO $ logInfo $ "Making text louder: " ++ show msg
    
    let new = msg <> "!"
    pure $ messageView new


messageView :: Text -> View Message ()
messageView msg = do
  row id $ do
    -- Log when button is clicked
    button (Louder msg) id "Louder"
    el_ $ text msg
