{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TemplateHaskell     #-}
module Input where


import Pipe

import Lens.Micro.Platform

import Graphics.Gloss.Interface.IO.Game(Event(..), Key(..), MouseButton(..), KeyState(..))

data Zooming = ZoomOut | ZoomIn
  deriving (Eq, Ord, Show)

data Jump = JumpUp | JumpDown
  deriving (Eq, Ord, Show)


data Input = Input
  { _dragging :: Maybe (Float, Float)
    -- ^ Nothing if not dragging, Just (x, y) when we are dragging,
    -- where (x, y) is the position we started to drag from
  , _translation :: (Float, Float)
  , _zooming :: Maybe Zooming
  , _save_screenshot :: Bool
  , _show_guides :: Bool
  , _show_points :: Bool
  , _jump :: Maybe Jump
  }
  deriving (Eq, Ord, Show)

makeLenses ''Input

initial :: Input
initial =
  Input
  { _dragging = Nothing
  , _zooming = Nothing
  , _translation = (0, 0)
  , _save_screenshot = False
  , _show_guides = False
  , _show_points = True
  , _jump = Nothing
  }



-- | Transforms user actions on the keyboard + mouse
-- to a more abstract 'input' format
-- that makes sense across frames.
-- (i.e. abstracting individual events into things like dragging and toggling.)
handleEvent :: Event -> Input -> Input
handleEvent event input =
  case event of
    EventKey (MouseButton LeftButton) Up _ _ ->
      input{_dragging = Nothing}
    EventKey (MouseButton LeftButton) Down _ pos ->
      input{_dragging = Just pos, _translation = (0, 0)}
    EventMotion (x, y) ->
      case input^.dragging of
        Nothing ->
          input
        Just (x0, y0) ->
          input
          |> set dragging (Just (x, y))
          |> (translation._1) +~ (x - x0)
          |> (translation._2) +~ (y - y0)
    EventKey (MouseButton WheelUp) _ _ _ ->
      input{_zooming = Just ZoomIn}
    EventKey (MouseButton WheelDown) _ _ _ ->
      input{_zooming = Just ZoomOut}
    EventKey (Char 's') Down _ _ ->
      input{_save_screenshot = True}
    EventKey (Char 'p') Down _ _ ->
      input
      |> toggle show_points
    EventKey (Char 'g') Down _ _ ->
      input
      |> toggle show_guides
    EventKey (Char '=') Down _ _ ->
      input
      |> set jump (Just JumpUp)
    EventKey (Char '-') Down _ _ ->
      input
      |> set jump (Just JumpDown)
    _ ->
      input
  where
    toggle attribute = over attribute not
