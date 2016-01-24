{-# LANGUAGE RecursiveDo #-}

-- | Functions on events
module Reactive.Banana.SDL2.Util ( addHandler, fire, sdlEvent, tickEvent, tickDiffEvent
                                , keyEvent, keyDownEvent, keyUpEvent, mouseEvent, mouseButtonEvent
                                , keyFilter, keyUpFilter
                                , mouseEventWithin
                                , whileM, successive ) where

import           Control.Monad              (liftM, when)
import           Reactive.Banana            as R
import           Reactive.Banana.Frameworks
import           Reactive.Banana.SDL2.Types
import           SDL
import           SDL.Raw                    as SDLR

-- | run while the given computation returns True
whileM :: IO Bool -> IO ()
whileM f = f >>= (\x -> when x $ whileM f)

-- | get the AddHandler from a EventSource
addHandler :: EventSource a -> AddHandler a
addHandler = fst

-- | fire the event from an Event Source
fire :: EventSource a -> a -> IO ()
fire = snd

-- | SDL event
sdlEvent :: SDLEventSource -> MomentIO WrappedEvent
sdlEvent = fromAddHandler . addHandler . getSDLEvent

-- | SDL tick
tickEvent :: SDLEventSource -> MomentIO TickEvent
tickEvent = fromAddHandler .  addHandler . getTickEvent

-- | event carrying the difference between the last two SDL ticks
tickDiffEvent :: SDLEventSource -> MomentIO TickEvent
tickDiffEvent source = mdo
  te <- tickEvent source
  s <- (successive (\a b-> if b > a then Just (b - a) else Nothing)) te
  return s

successive :: (a -> a -> Maybe b) -> R.Event a -> MomentIO (R.Event b)
successive f e = (\b -> filterJust (b <@> e)) <$> stepper (const Nothing) (f <$> e)
  -- Below same as about but with mdo; easier to debug (at least to me)
  --  mdo
  --    b <- stepperB f e
  --    return $ filterJust (b <@> e)
  --where
  --  stepperB :: (a -> a -> Maybe b) -> R.Event a -> MomentIO (Behavior (a -> Maybe b ))
  --  stepperB f e = stepper (const Nothing) (f <$> e)


-- | filter any key events
keyEvent :: WrappedEvent -> WrappedEvent
keyEvent =  filterE isKey
    where
        isKey e = case e of
            SDL.KeyboardEvent _ -> True
            otherwise -> False

-- | event carrying the key pressed down
keyDownEvent :: WrappedEvent -> R.Event SDL.Keysym
keyDownEvent= filterJust . (isDown <$>) . keyEvent
        where isDown (SDL.KeyboardEvent (KeyboardEventData _ Pressed _ k)) = Just k
              isDown _ = Nothing

-- | event carrying the key pressed up
keyUpEvent :: WrappedEvent -> R.Event SDL.Keysym
keyUpEvent = filterJust . (isDown <$>) . keyEvent
        where isDown (SDL.KeyboardEvent (KeyboardEventData _ Released _ k)) = Just k
              isDown _ = Nothing

-- | filter any mouse event (button or move)
mouseEvent :: WrappedEvent -> WrappedEvent
mouseEvent esdl = unionWith f mouseMotion (mouseButtonEvent esdl)
    where
        f e1 e2 = e2
        mouseMotion =  filterE isMotion $ esdl
        isMotion e = case e of
            SDL.MouseMotionEvent MouseMotionEventData {} -> True
            otherwise -> False

-- | mouse button event
mouseButtonEvent :: WrappedEvent -> WrappedEvent
mouseButtonEvent = filterE isButton
    where
        isButton e = case e of
            SDL.MouseButtonEvent MouseButtonEventData{} -> True
            otherwise -> False

-- | mouse event occuring inside a given area
mouseEventWithin :: Rect -> WrappedEvent -> WrappedEvent
mouseEventWithin ~(Rect x y w h) = filterE isWithin
    where
        within pos = undefined
        isWithin e = case e of
            SDL.MouseMotionEvent (MouseMotionEventData _ _ _ pos _) -> within pos
            SDL.MouseButtonEvent (MouseButtonEventData _ _ _ _ _ pos) -> within pos
            otherwise -> False


-- | filter an event on a particular key being held down
keyFilter :: SDL.Keycode-> SDL.EventPayload -> Bool
keyFilter k (SDL.KeyboardEvent (KeyboardEventData _ Pressed _ (SDL.Keysym _ k' _ )))
  | k == k'   = True
keyFilter _ _ = False

-- | filter an event on a particular key being released
keyUpFilter :: SDL.Keycode -> SDL.EventPayload -> Bool
keyUpFilter k (SDL.KeyboardEvent (KeyboardEventData _ Released _ (SDL.Keysym _ k' _ )))
  | k == k'     = True
keyUpFilter _ _ = False



