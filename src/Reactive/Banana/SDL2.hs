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

-- | one step in the main event loop, returning False when it needs to stop

mainSDLPump :: SDLEventSource -> IO Bool
mainSDLPump es= do
  let esdl = getSDLEvent es
      etick = getTickEvent es
  tick <- SDL.ticks
  me <- collectEvents

  case me of
    Nothing -> return False
    Just e -> do
      mapM (fire esdl) e
      fire etick tick
      return True

-- | collect SDL events
-- return Nothing on quit, otherwise the last event
collectEvents :: IO (Maybe [SDL.EventPayload])
collectEvents = do
      e <- SDL.pollEvent
      case fmap eventPayload e of
        Just SDL.QuitEvent -> return Nothing
        Nothing -> return (Just [])
        Just ev -> liftM (liftM (ev:)) collectEvents

runSDLPump :: SDLEventSource -> IO ()
runSDLPump es = whileM (mainSDLPump es)

runCappedSDLPump = undefined

