-- | TreeLayout is a layout form intended for visualizing tree-data,
-- such as program code.
--
-- Its design goals are:
--
-- * Make good use of the available screen real-estate.
-- * Avoid horizontal scroll
-- * Display the hierarchy/tree structure clearly
-- * Make the layout changes due to edits predictable and easy to follow
--
-- Subtrees are laid out horizontally as long as they fit within the
-- available horizontal space, to avoid horizontal scrolling.
--
-- When there is not enough horizontal space to lay the entire tree
-- horizontally, vertical layouts are used for the upper parts of the tree.
--
-- Hierarchy disambiguation happens using parentheses and indentation,
-- but only when necessary. For example: a horizontally laid out child
-- of a vertically laid out parent will not use parentheses as the
-- hierarchy is already clear in the layout itself.

{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module Graphics.UI.Bottle.Widget.TreeLayout
    ( TreeLayout(..), render

    -- * Layout params
    , LayoutParams(..), layoutMode, layoutContext
    , LayoutMode(..), _LayoutNarrow, _LayoutWide
    , LayoutDisambiguationContext(..)

    -- * Lenses
    , alignedWidget, widget, alignment, modeWidths

    -- * Leaf generation
    , fromAlignedWidget, fromCenteredWidget, fromCenteredView, empty

    -- * Operations
    , pad
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Vector.Vector2 (Vector2)
import           Graphics.UI.Bottle.Alignment (Alignment)
import           Graphics.UI.Bottle.View (View)
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widget.Aligned (AlignedWidget)
import qualified Graphics.UI.Bottle.Widget.Aligned as AlignedWidget

import           Prelude.Compat

data LayoutMode
    = LayoutNarrow Widget.R -- ^ limited by the contained width field
    | LayoutWide -- ^ no limit on width
Lens.makePrisms ''LayoutMode

modeWidths :: Lens.Traversal' LayoutMode Widget.R
modeWidths _ LayoutWide = pure LayoutWide
modeWidths f (LayoutNarrow limit) = f limit <&> LayoutNarrow

-- The relevant context for knowing whether parenthesis/indentation is needed
data LayoutDisambiguationContext
    = LayoutClear
    | LayoutHorizontal
    | LayoutVertical

data LayoutParams = LayoutParams
    { _layoutMode :: LayoutMode
    , _layoutContext :: LayoutDisambiguationContext
    }
Lens.makeLenses ''LayoutParams

newtype TreeLayout a = TreeLayout
    { _render :: LayoutParams -> AlignedWidget a
    }
Lens.makeLenses ''TreeLayout

alignedWidget ::
    Lens.Setter
    (TreeLayout a) (TreeLayout b)
    (AlignedWidget a) (AlignedWidget b)
alignedWidget = render . Lens.mapped

widget :: Lens.Setter (TreeLayout a) (TreeLayout b) (Widget a) (Widget b)
widget = alignedWidget . AlignedWidget.widget

alignment :: Lens.Setter' (TreeLayout a) Alignment
alignment = alignedWidget . AlignedWidget.alignment

-- | Lifts a Widget into a 'TreeLayout'
fromAlignedWidget :: AlignedWidget a -> TreeLayout a
fromAlignedWidget = TreeLayout . const

-- | Lifts a Widget into a 'TreeLayout' with an alignment point at the
-- widget's center
fromCenteredWidget :: Widget a -> TreeLayout a
fromCenteredWidget = fromAlignedWidget . AlignedWidget.fromCenteredWidget

-- | Lifts a View into a 'TreeLayout' with an alignment point at the
-- view's center
fromCenteredView :: View -> TreeLayout a
fromCenteredView = fromCenteredWidget . Widget.fromView

-- | The empty 'TreeLayout'
empty :: TreeLayout a
empty = fromCenteredView View.empty

-- | Adds space around a given 'TreeLayout'. Each of the 'Vector2'
-- components is added to the size twice (once on each side). Only the
-- width component of the 'Vector2' affects layout decisions by
-- shrinking the width available to the given 'TreeLayout'.
pad :: Vector2 Widget.R -> TreeLayout a -> TreeLayout a
pad p w =
    w
    & render . Lens.argument . layoutMode . modeWidths -~ 2 * (p ^. _1)
    & alignedWidget %~ AlignedWidget.pad p
