module Editor.BottleWidgets
  ( makeTextView, makeLabel
  , makeFocusableView, makeFocusableTextView
  , wrapDelegatedOT, wrapDelegatedVA
  , makeTextEdit, makeLineEdit, makeWordEdit
  , makeNameEdit, nameSrcTint
  , hboxAlign, vboxAlign
  , hboxSpaced, hboxCenteredSpaced
  , hboxCentered, vboxCentered
  , hbox, vbox
  , gridHSpaced, gridHSpacedCentered
  , stdSpaceWidget
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (when, liftM)
import Data.ByteString.Char8 (pack)
import Data.List (intersperse)
import Data.Maybe (isJust)
import Data.Monoid (mappend)
import Data.Store.Guid (Guid)
import Editor.CodeEdit.VarAccess (VarAccess, WidgetT)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.CodeEdit.VarAccess as VarAccess
import qualified Editor.Config as Config
import qualified Editor.ITransaction as IT
import qualified Editor.Layers as Layers
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

makeTextView :: Monad m => String -> AnimId -> OTransaction t m (Widget f)
makeTextView text myId = do
  style <- OT.readTextStyle
  return $
    TextView.makeWidget (TextEdit.sTextViewStyle style) text myId

makeLabel :: MonadF m => String -> AnimId -> OTransaction t m (Widget f)
makeLabel text prefix =
  makeTextView text $ mappend prefix [pack text]

makeFocusableView
  :: (Applicative f, MonadF m)
  => Widget.Id -> Widget f
  -> OTransaction t m (Widget f)
makeFocusableView myId widget = do
  hasFocus <- liftM isJust $ OT.subCursor myId
  let
    setBackground
      | hasFocus = Widget.backgroundColor Layers.cursorBG WidgetIds.backgroundCursorId Config.cursorBGColor
      | otherwise = id
  return .
    (Widget.atWIsFocused . const) hasFocus . setBackground $
    Widget.takesFocus (const (pure myId)) widget

makeFocusableTextView
  :: (Applicative f, MonadF m)
  => String -> Widget.Id
  -> OTransaction t m (Widget f)
makeFocusableTextView text myId = do
  textView <- makeTextView text $ Widget.toAnimId myId
  makeFocusableView myId textView

fdStyle :: FocusDelegator.Style
fdStyle = FocusDelegator.Style
  { FocusDelegator.color = Config.cursorBGColor
  , FocusDelegator.layer = Layers.cursorBG
  , FocusDelegator.cursorBGAnimId = WidgetIds.backgroundCursorId
  }

wrapDelegatedWith
  :: (Applicative f, Monad m)
  => m Widget.Id
  -> ((Widget.Id -> Widget.Id) -> m a -> m a)
  -> FocusDelegator.Config
  -> FocusDelegator.IsDelegating
  -> ((Widget f -> Widget f) -> a -> b)
  -> (Widget.Id -> m a)
  -> Widget.Id -> m b
wrapDelegatedWith readCursor atCursor config entryState aToB mkA myId = do
  cursor <- readCursor
  FocusDelegator.wrapEnv (FocusDelegator.Env config fdStyle) entryState mk myId cursor
  where
    mk f innerId newCursor =
      liftM (aToB f) . (atCursor . const) newCursor $ mkA innerId

-- TODO: This logic belongs in the FocusDelegator itself
wrapDelegatedOT
  :: (Applicative f, Monad m)
  => FocusDelegator.Config
  -> FocusDelegator.IsDelegating
  -> ((Widget f -> Widget f) -> a -> b)
  -> (Widget.Id -> OTransaction t m a)
  -> Widget.Id -> OTransaction t m b
wrapDelegatedOT = wrapDelegatedWith OT.readCursor (OT.atEnv . OT.atEnvCursor)

wrapDelegatedVA ::
  (Applicative f, Monad m) =>
  FocusDelegator.Config ->
  FocusDelegator.IsDelegating ->
  ((Widget f -> Widget f) -> a -> b) ->
  (Widget.Id -> VarAccess m a) ->
  Widget.Id -> VarAccess m b
wrapDelegatedVA =
  wrapDelegatedWith (VarAccess.otransaction OT.readCursor) (VarAccess.atEnv . OT.atEnvCursor)

makeTextEdit
  :: Monad m
  => Transaction.Property t m String
  -> Widget.Id
  -> OTransaction t m (OT.WidgetT t m)
makeTextEdit textRef myId = do
  cursor <- OT.readCursor
  style <- OT.readTextStyle
  return .
    Widget.atEvents setter $
    TextEdit.make style cursor (Property.value textRef) myId
  where
    setter (newText, eventRes) = IT.transaction $ do
      when (newText /= Property.value textRef) $ Property.set textRef newText
      return eventRes

removeKey
  :: (Monad m)
  => (a -> b -> m (Widget f))
  -> EventMap.ModKey
  -> a -> b -> m (Widget f)
removeKey makeEdit key =
  (fmap . fmap . liftM . Widget.atWEventMap)
  (EventMap.deleteKey (EventMap.KeyEvent EventMap.Press key))
  makeEdit

makeLineEdit ::
  Monad m =>
  Transaction.Property t m String ->
  Widget.Id ->
  OTransaction t m (OT.WidgetT t m)
makeLineEdit =
  removeKey makeTextEdit $
  EventMap.ModKey EventMap.noMods EventMap.KeyEnter

makeWordEdit ::
  Monad m =>
  Transaction.Property t m String ->
  Widget.Id ->
  OTransaction t m (OT.WidgetT t m)
makeWordEdit =
  removeKey makeLineEdit $
  EventMap.ModKey EventMap.noMods EventMap.KeySpace

makeNameEdit ::
  Monad m => (VarAccess.NameSource, String) -> Guid -> Widget.Id -> VarAccess m (WidgetT m)
makeNameEdit (nameSrc, name) ident myId =
  liftM (nameSrcTint nameSrc) .
  (VarAccess.atEnv . OT.atEnvTextStyle)
  ((TextEdit.atSEmptyUnfocusedString . const) name .
   (TextEdit.atSEmptyFocusedString . const) (concat ["<", name, ">"])) $
  VarAccess.otransaction . flip makeEditor myId =<<
  VarAccess.transaction (Anchors.assocNameRef ident)
  where
    makeEditor =
      (fmap . fmap . liftM . Widget.atWEventMap)
      (EventMap.filterChars (`notElem` "[]\\`"))
      makeWordEdit

nameSrcTint :: VarAccess.NameSource -> Widget f -> Widget f
nameSrcTint VarAccess.AutoGeneratedName = Widget.tint Config.autoGeneratedNameTint
nameSrcTint VarAccess.StoredName = id

boxAlign :: Box.Orientation -> Box.Alignment -> [Widget f] -> Widget f
boxAlign orientation align =
  Box.toWidget .
  Box.makeAlign align orientation

hboxAlign :: Box.Alignment -> [Widget f] -> Widget f
hboxAlign = boxAlign Box.horizontal

vboxAlign :: Box.Alignment -> [Widget f] -> Widget f
vboxAlign = boxAlign Box.vertical

vboxCentered :: [Widget f] -> Widget f
vboxCentered = vboxAlign 0.5

hboxCentered :: [Widget f] -> Widget f
hboxCentered = hboxAlign 0.5

hbox :: [(Box.Alignment, Widget f)] -> Widget f
hbox = Box.toWidget . Box.make Box.horizontal

vbox :: [(Box.Alignment, Widget f)] -> Widget f
vbox = Box.toWidget . Box.make Box.vertical

stdSpaceWidget :: Widget f
stdSpaceWidget = uncurry Widget.liftView $ Spacer.makeHorizontal 20

hboxSpaced :: [(Box.Alignment, Widget f)] -> Widget f
hboxSpaced = hbox . intersperse (0.5, stdSpaceWidget)

hboxCenteredSpaced :: [Widget f] -> Widget f
hboxCenteredSpaced = hboxAlign 0.5 . intersperse stdSpaceWidget

gridHSpaced :: [[(Grid.Alignment, Widget f)]] -> Widget f
gridHSpaced = Grid.toWidget . Grid.make . map (intersperse (0, stdSpaceWidget))

gridHSpacedCentered :: [[Widget f]] -> Widget f
gridHSpacedCentered = gridHSpaced . (map . map) ((,) 0.5)
