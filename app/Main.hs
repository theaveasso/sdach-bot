module Main (main) where

import Lib
import Telegram.Bot.Simple

data Model = Model
data Action = DoNothing

bot :: BotApp Model Action
bot = BotApp {botJobs=_botJobs, botInitialModel=_botInitialModel, botHandler=_botHandler, botAction=_botAction}

main :: IO ()
main = someFunc
