{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib

import           Data.Text                        (Text)
import qualified Data.Text                        as Text
-- import           Data.Maybe

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.Debug
import Telegram.Bot.Simple.UpdateParser

import System.Environment (getEnv)
import Configuration.Dotenv (defaultConfig, loadFile)

data Model = Model deriving (Show)
data Action =
    DoNothing
  | Echo Text

  deriving (Show)

sdachBot :: BotApp Model Action
sdachBot = BotApp {
  botInitialModel = Model,
  botAction = flip handleUpdate,
  botHandler = handleAction,
  botJobs = []
}

handleUpdate :: Model -> Update -> Maybe Action
handleUpdate _ = parseUpdate
  (Echo <$> text)

handleAction :: Action -> Model -> Eff Action Model
handleAction actions model = case actions of
  DoNothing  -> pure model
  Echo msg -> model <# do
    replyText $ "sdach " `Text.append` msg
    pure DoNothing
    
run :: Token -> IO ()
run apiToken = do
  _apiToken <- defaultTelegramClientEnv apiToken
  startBot_ (traceBotDefault sdachBot) _apiToken

main :: IO ()
main = do
  loadFile defaultConfig
  token <- Token . Text.pack <$> getEnv "API_BOT_TOKEN"
  run token
