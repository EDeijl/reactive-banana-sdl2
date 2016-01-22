module Reactive.Banana.SDL2 ( module Reactive.Banana.SDL2.Types
                            , module Reactive.Banana.SDL2.Util
                            , getSDLEventSource, runSDLPump
                            )where

import           Control.Monad
import           Data.Word
import           Reactive.Banana            as R
import           Reactive.Banana.Frameworks (newAddHandler)
import           Reactive.Banana.SDL2.Types
import           Reactive.Banana.SDL2.Util
import           SDL
import           SDL.Time
import qualified SDL.Raw                    as SDLR


getSDLEventSource :: IO SDLEventSource
getSDLEventSource = SDLEventSource <$> newAddHandler <*> newAddHandler

mainSDLPump :: SDLEventSource -> IO Bool
mainSDLPump es= do
  let esdl = getSDLEvent es
      etick = getTickEvent es
  tick <- SDL.ticks
  me <- collectEvents
  case me of
    Nothing -> return False
    Just e -> do
      fire esdl e
      fire etick tick
      return True
  --case me of


    --Nothing -> return False
    --e -> do
      --fire esdl e
      --fire etick tick
      --return True

collectEvents :: IO (Maybe SDL.EventPayload)
collectEvents = do
  e <- SDL.pollEvent
  case fmap eventPayload e of
    Just SDL.QuitEvent -> return Nothing
    Just ed -> return (Just ed)

runSDLPump :: SDLEventSource -> IO ()
runSDLPump es = whileM (mainSDLPump es)

runCappedSDLPump = undefined

