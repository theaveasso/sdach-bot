{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Text                        (Text)
import     qualified Data.Text as Text
import           Data.Maybe
import Data.Time
import    Control.Monad.Trans

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.Debug
import Telegram.Bot.Simple.UpdateParser

  import Control.Applicative
import System.Environment (getEnv)
import Configuration.Dotenv (defaultConfig, loadFile)

newtype TodoItem = TodoItem {todoItemTitle :: Text} deriving (Show)
newtype Model = Model {todoItems :: [TodoItem]} deriving (Show)

initialModel :: Model
initialModel = Model {todoItems = []}

-- make todo by task name
makeTodoItem :: Text -> TodoItem
makeTodoItem task = TodoItem {todoItemTitle = task}
-- add a new todo to the model
addTodo :: TodoItem -> Model -> Model
addTodo item model = model {todoItems = item : todoItems model}
-- show todo item from the model
showTodo :: TodoItem -> Text
showTodo task = "# " <> todoItemTitle task <> "\n"

showTodos :: Model -> Text
showTodos model = 
  case foldMap showTodo (todoItems model) of
    ""    -> "No things to do yet. Would you like to add a new task? :"
    items -> "Some things for you to do:\n" <> items

data Action =
    DoNothing
  | AddTodo Text
  | ShowTodos
  | ShowTime
  deriving (Show)

sdachBot :: BotApp Model Action
sdachBot = BotApp {
  botInitialModel = Model [],
  botAction = flip handleUpdate,
  botHandler = handleAction,
  botJobs = []
}

handleUpdate :: Model -> Update -> Maybe Action
handleUpdate _ = parseUpdate
  $ ShowTodos <$ command "show"
  <|> ShowTime <$ command "time"
  <|> AddTodo <$> text

handleAction :: Action -> Model -> Eff Action Model
handleAction actions model = case actions of
  DoNothing  -> pure model
  AddTodo task -> addTodo (makeTodoItem task) model <# do
    replyText "Todo Added"
    pure DoNothing
  ShowTodos -> model <# do
    replyText (showTodos model)
    pure DoNothing
  ShowTime -> model <# do
    now <- liftIO getCurrentTime
    replyText $ Text.pack (show now)
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
