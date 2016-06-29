{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings, RankNTypes, TypeFamilies, LambdaCase, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.GUI.ExpressionGui
    ( ExpressionGuiM(..)
    , ExpressionGui, toLayout, egWidget, egAlignment, egAbsWidget
      , ExprGuiT.egLayout, ExprGuiT.fromLayout, egIsFocused
    , LayoutMode(..), LayoutParams(..), LayoutDisambiguationContext(..)
    -- General:
    , ExprGuiT.fromValueWidget
    , pad
    , stdHSpace, stdVSpace
    , combine, combineSpaced
    , (||>), (<||)
    , vboxTopFocal, vboxTopFocalSpaced
    , horizVertFallback
    , tagItem
    , listWithDelDests
    , makeLabel
    , grammarLabel
    , addValBG, addValFrame, addValPadding
    , addValBGWithColor
    , liftLayers
    -- Lifted widgets:
    , makeFocusDelegator
    , makeFocusableView
    , makeNameView
    , makeNameEdit, makeNameEditWith
    , makeNameOriginEdit
    , deletionDiagonal
    -- Info adding
    , annotationSpacer
    , NeighborVals(..)
    , EvalAnnotationOptions(..), maybeAddAnnotationWith
    , WideAnnotationBehavior(..), wideAnnotationBehaviorFromSelected
    , evaluationResult
    -- Expression wrapping
    , MyPrecedence(..), ParentPrecedence(..), Precedence(..), Precedence.precBefore, Precedence.precAfter
    , maybeAddAnnotationPl
    , stdWrap
    , parentDelegator
    , stdWrapParentExpr
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Binary.Utils (encodeS)
import           Data.CurAndPrev (CurAndPrev(..), CurPrevTag(..), curPrevTag, fallbackToPrev)
import qualified Data.List as List
import qualified Data.List.Utils as ListUtils
import           Data.Maybe (fromMaybe)
import           Data.Store.Property (Property(..))
import           Data.Store.Transaction (Transaction)
import           Data.String (IsString(..))
import           Data.Traversable.Generalized (GTraversable)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget, WidgetF)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import           Graphics.UI.Bottle.Widgets.Layout (Layout)
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Graphics.UI.GLFW as GLFW
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Eval.Results as ER
import qualified Lamdu.GUI.CodeEdit.Settings as CESettings
import qualified Lamdu.GUI.EvalView as EvalView
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui.Types ( ExpressionGuiM(..), ExpressionGui
                                               , ShowAnnotation(..), EvalModeShow(..)
                                               , egWidget, egAlignment, egAbsWidget
                                               , modeWidths
                                               , LayoutMode(..)
                                               , LayoutParams(..)
                                               , layoutMode, layoutContext
                                               , LayoutDisambiguationContext(..)
                                               , toLayout
                                               )
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import           Lamdu.GUI.Precedence (MyPrecedence(..), ParentPrecedence(..), Precedence(..))
import qualified Lamdu.GUI.Precedence as Precedence
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Style as Style
import           Lamdu.Sugar.Names.Types (Name(..), NameSource(..), NameCollision(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Prelude.Compat

type T = Transaction

{-# INLINE egIsFocused #-}
egIsFocused :: ExpressionGui m -> Bool
-- TODO: Fix this:
egIsFocused (ExpressionGui mkLayout) =
    mkLayout params & Widget.isFocused
    where
        params =
            LayoutParams
            { _layoutMode = LayoutWide
            , _layoutContext = LayoutClear
            }

pad :: Vector2 Widget.R -> ExpressionGui m -> ExpressionGui m
pad p =
    toLayout %~ f
    where
        f mkLayout layoutParams =
            layoutParams
            & layoutMode . modeWidths -~ 2 * (p ^. _1)
            & mkLayout
            & Layout.pad p

maybeIndent :: Maybe ParenIndentInfo -> ExpressionGui m -> ExpressionGui m
maybeIndent mPiInfo gui =
    ExpressionGui $
    \lp ->
    case (lp ^. layoutContext, mPiInfo) of
    (LayoutVertical, Just piInfo) ->
        let indentConf = piIndentConfig piInfo
            stdSpace = piStdHorizSpacing piInfo
            barWidth = stdSpace * Config.indentBarWidth indentConf
            gapWidth = stdSpace * Config.indentBarGap indentConf
            indentWidth = barWidth + gapWidth
            content =
                lp & layoutMode . modeWidths -~ indentWidth
                & gui ^. toLayout
                & Layout.alignment . _2 .~ 0
            bgAnimId = piAnimId piInfo ++ ["("]
        in
        content
        & Layout.addBefore Layout.Horizontal
            [ Spacer.make
                (Vector2 barWidth (content ^. Widget.height))
                & Widget.fromView
                & Widget.backgroundColor 0 bgAnimId
                    (Config.indentBarColor indentConf)
                & Layout.fromCenteredWidget
                & Layout.alignment . _2 .~ 0
            , Spacer.make (Vector2 gapWidth 0)
                & Widget.fromView & Layout.fromCenteredWidget
            ]
    _ -> lp & gui ^. toLayout

vboxTopFocal :: [ExpressionGui m] -> ExpressionGui m
vboxTopFocal [] = ExprGuiT.fromLayout Layout.empty
vboxTopFocal (ExpressionGui mkLayout:guis) =
    ExpressionGui $
    \layoutParams ->
    let cp =
            LayoutParams
            { _layoutMode = layoutParams ^. layoutMode
            , _layoutContext = LayoutVertical
            }
    in
    mkLayout cp
    & Layout.addAfter Layout.Vertical
        (guis ^.. Lens.traverse . toLayout ?? cp)

vboxTopFocalSpaced ::
    Monad m => ExprGuiM m ([ExpressionGui f] -> ExpressionGui f)
vboxTopFocalSpaced =
    stdVSpace
    <&> ExprGuiT.fromValueWidget
    <&> List.intersperse
    <&> fmap vboxTopFocal

hCombine ::
    (Layout.Orientation ->
     [Layout (T f Widget.EventResult)] ->
     Layout (T f Widget.EventResult) ->
     Layout (T f Widget.EventResult)) ->
    Layout (T f Widget.EventResult) -> ExpressionGui f -> ExpressionGui f
hCombine f layout gui =
    ExpressionGui $
    \layoutParams ->
    LayoutParams
    { _layoutMode =
        layoutParams ^. layoutMode & modeWidths -~ layout ^. Widget.width
    , _layoutContext = LayoutHorizontal
    }
    & gui ^. toLayout
    & f Layout.Horizontal [layout]

(||>) :: Layout (T f Widget.EventResult) -> ExpressionGui f -> ExpressionGui f
(||>) = hCombine Layout.addBefore

(<||) :: ExpressionGui f -> Layout (T f Widget.EventResult) -> ExpressionGui f
(<||) = flip (hCombine Layout.addAfter)

stdHSpace :: Monad m => ExprGuiM m (Widget a)
stdHSpace =
    ExprGuiM.widgetEnv BWidgets.stdHSpaceView
    <&> Widget.fromView

stdVSpace :: Monad m => ExprGuiM m (Widget a)
stdVSpace =
    ExprGuiM.widgetEnv BWidgets.stdVSpaceView
    <&> Widget.fromView

data ParenIndentInfo = ParenIndentInfo
    { piAnimId :: AnimId
    , piTextStyle :: TextView.Style
    , piIndentConfig :: Config.Indent
    , piStdHorizSpacing :: Widget.R
    }

parenLabel :: ParenIndentInfo -> String -> Layout a
parenLabel parenInfo t =
    TextView.make (piTextStyle parenInfo) t
    (piAnimId parenInfo ++ [fromString t])
    & Widget.fromView & Layout.fromCenteredWidget

horizVertFallback ::
    Monad m =>
    Maybe AnimId ->
    ExprGuiM m (ExpressionGui f -> ExpressionGui f -> ExpressionGui f)
horizVertFallback mParenId =
    mParenId & Lens._Just %%~ makeParenIndentInfo
    <&> horizVertFallbackH

horizVertFallbackH ::
    Maybe ParenIndentInfo ->
    ExpressionGui m -> ExpressionGui m -> ExpressionGui m
horizVertFallbackH mParenInfo horiz vert =
    ExpressionGui $
    \layoutParams ->
    let wide = layoutParams & layoutMode .~ LayoutWide & horiz ^. toLayout
    in
    case layoutParams ^. layoutMode of
    LayoutWide ->
        case (mParenInfo, layoutParams ^. layoutContext) of
        (Just parenInfo, LayoutHorizontal) ->
            wide
            & Layout.addBefore Layout.Horizontal [parenLabel parenInfo "("]
            & Layout.addAfter Layout.Horizontal [parenLabel parenInfo ")"]
        _ -> wide
    LayoutNarrow limit
        | wide ^. Widget.width > limit ->
            layoutParams & maybeIndent mParenInfo vert ^. toLayout
        | otherwise -> wide

combineWith ::
    Maybe ParenIndentInfo ->
    ([Layout (T m Widget.EventResult)] -> [Layout (T m Widget.EventResult)]) ->
    ([ExpressionGui m] -> [ExpressionGui m]) ->
    [ExpressionGui m] -> ExpressionGui m
combineWith mParenInfo onHGuis onVGuis guis =
    horizVertFallbackH mParenInfo wide vert
    where
        vert = vboxTopFocal (onVGuis guis <&> egAlignment . _1 .~ 0)
        wide =
            guis ^.. Lens.traverse . toLayout
            ?? LayoutParams
                { _layoutMode = LayoutWide
                , _layoutContext = LayoutHorizontal
                }
            & onHGuis
            & Layout.hbox 0.5
            & const & ExpressionGui

combine :: [ExpressionGui m] -> ExpressionGui m
combine = combineWith Nothing id id

makeParenIndentInfo :: Monad m => AnimId -> ExprGuiM m ParenIndentInfo
makeParenIndentInfo parensId =
    do
        textStyle <-
            ExprGuiM.widgetEnv WE.readTextStyle
            <&> (^. TextEdit.sTextViewStyle)
        conf <- ExprGuiM.readConfig <&> Config.indent
        stdSpacing <- ExprGuiM.widgetEnv BWidgets.stdSpacing <&> (^. _1)
        ParenIndentInfo parensId textStyle conf stdSpacing & return

combineSpaced ::
    Monad m => Maybe AnimId -> ExprGuiM m ([ExpressionGui f] -> ExpressionGui f)
combineSpaced mParensId =
    do
        hSpace <- stdHSpace <&> Layout.fromCenteredWidget
        vSpace <- stdVSpace <&> ExprGuiT.fromValueWidget
        mParenInfo <- mParensId & Lens._Just %%~ makeParenIndentInfo
        return $ combineWith mParenInfo (List.intersperse hSpace) (List.intersperse vSpace)

tagItem :: Monad m => ExprGuiM m (Layout (T f Widget.EventResult) -> ExpressionGui f -> ExpressionGui f)
tagItem =
    stdHSpace <&> Layout.fromCenteredWidget <&> f
    where
        f space tag item =
            ExpressionGui $
            \layoutParams ->
            let remainingLayoutMode =
                    layoutParams & layoutMode . modeWidths
                        -~ tagAndSpace ^. Widget.width
            in  tagAndSpace
                & Layout.addAfter Layout.Horizontal [remainingLayoutMode & item ^. toLayout]
            where
                tagAndSpace =
                    space
                    & Layout.addBefore Layout.Horizontal [tag]

addAnnotationBackgroundH :: (Config -> Draw.Color) -> Config -> AnimId -> Layout a -> Layout a
addAnnotationBackgroundH getColor config animId =
    Widget.backgroundColor bgLayer bgAnimId bgColor
    where
        bgAnimId = animId ++ ["annotation background"]
        bgLayer = Config.layerAnnotations $ Config.layers config
        bgColor = getColor config

addAnnotationBackground :: Config -> AnimId -> Layout a -> Layout a
addAnnotationBackground = addAnnotationBackgroundH Config.valAnnotationBGColor

addAnnotationHoverBackground :: Config -> AnimId -> Layout a -> Layout a
addAnnotationHoverBackground = addAnnotationBackgroundH Config.valAnnotationHoverBGColor

data WideAnnotationBehavior
    = ShrinkWideAnnotation
    | HoverWideAnnotation
    | KeepWideAnnotation

wideAnnotationBehaviorFromSelected :: Bool -> WideAnnotationBehavior
wideAnnotationBehaviorFromSelected False = ShrinkWideAnnotation
wideAnnotationBehaviorFromSelected True = HoverWideAnnotation

-- NOTE: Also adds the background color, because it differs based on
-- whether we're hovering
applyWideAnnotationBehavior ::
    Monad m =>
    AnimId -> WideAnnotationBehavior ->
    ExprGuiM m (Vector2 Widget.R -> Layout a -> Layout a)
applyWideAnnotationBehavior _ KeepWideAnnotation = return (const id)
applyWideAnnotationBehavior animId ShrinkWideAnnotation =
    ExprGuiM.readConfig
    <&>
    \config shrinkRatio layout ->
    Layout.scaleAround (Vector2 0.5 0) shrinkRatio layout
    & addAnnotationBackground config animId
applyWideAnnotationBehavior animId HoverWideAnnotation =
    do
        config <- ExprGuiM.readConfig
        lifter <- liftLayers
        shrinker <- applyWideAnnotationBehavior animId ShrinkWideAnnotation
        return $
            \shrinkRatio layout ->
                lifter layout
                & addAnnotationHoverBackground config animId
                & (`Layout.hoverInPlaceOf` shrinker shrinkRatio layout)

processAnnotationGui ::
    Monad m =>
    AnimId -> WideAnnotationBehavior ->
    ExprGuiM m (Widget.R -> Layout a -> Layout a)
processAnnotationGui animId wideAnnotationBehavior =
    f
    <$> ExprGuiM.readConfig
    <*> ExprGuiM.widgetEnv BWidgets.stdSpacing
    <*> applyWideAnnotationBehavior animId wideAnnotationBehavior
    where
        f config stdSpacing applyWide minWidth annotationLayout
            | annotationWidth > minWidth + max shrinkAtLeast expansionLimit
            || heightShrinkRatio < 1 =
                applyWide shrinkRatio annotationLayout
            | otherwise =
                maybeTooNarrow annotationLayout
                & addAnnotationBackground config animId
            where
                annotationWidth = annotationLayout ^. Widget.width
                expansionLimit =
                    Config.valAnnotationWidthExpansionLimit config & realToFrac
                maxWidth = minWidth + expansionLimit
                shrinkAtLeast = Config.valAnnotationShrinkAtLeast config & realToFrac
                heightShrinkRatio =
                    Config.valAnnotationMaxHeight config * stdSpacing ^. _2
                    / annotationLayout ^. Widget.height
                    & min 1
                shrinkRatio =
                    annotationWidth - shrinkAtLeast & min maxWidth & max minWidth
                    & (/ annotationWidth) & pure
                    & _2 %~ min heightShrinkRatio
                maybeTooNarrow
                    | minWidth > annotationWidth = Layout.pad (Vector2 ((minWidth - annotationWidth) / 2) 0)
                    | otherwise = id

data EvalResDisplay = EvalResDisplay
    { erdScope :: ER.ScopeId
    , erdSource :: CurPrevTag
    , erdVal :: ER.Val Type
    }

makeEvaluationResultView ::
    Monad m => AnimId -> EvalResDisplay -> ExprGuiM m (Layout a)
makeEvaluationResultView animId res =
    do
        config <- ExprGuiM.readConfig
        view <- EvalView.make (animId ++ [encodeS (erdScope res)]) (erdVal res)
        view
            & case erdSource res of
            Current -> id
            Prev -> View.tint (Config.staleResultTint (Config.eval config))
            & return
    <&> Widget.fromView
    <&> Layout.fromCenteredWidget

makeTypeView :: Monad m => Type -> AnimId -> ExprGuiM m (Layout f)
makeTypeView typ animId =
    TypeView.make typ animId <&> Layout.fromCenteredWidget . Widget.fromView

data NeighborVals a = NeighborVals
    { prevNeighbor :: a
    , nextNeighbor :: a
    } deriving (Functor, Foldable, Traversable)

makeEvalView ::
    Monad m =>
    NeighborVals (Maybe EvalResDisplay) -> EvalResDisplay ->
    AnimId -> ExprGuiM m (Layout a)
makeEvalView (NeighborVals mPrev mNext) evalRes animId =
    do
        config <- ExprGuiM.readConfig
        let Config.Eval{..} = Config.eval config
        let makeEvaluationResultViewBG res =
                makeEvaluationResultView animId res
                <&> addAnnotationBackground config (animId ++ [encodeS (erdScope res)])
        let neighbourViews n yPos =
                n ^.. Lens._Just
                <&> makeEvaluationResultViewBG
                <&> Lens.mapped %~
                    Layout.pad (neighborsPadding <&> realToFrac) .
                    Widget.scale (neighborsScaleFactor <&> realToFrac)
                <&> Lens.mapped . Layout.alignment . _2 .~ yPos
        prevs <- neighbourViews mPrev 1 & sequence
        nexts <- neighbourViews mNext 0 & sequence
        evalView <- makeEvaluationResultView animId evalRes
        evalView
            & Layout.addBefore Layout.Horizontal prevs
            & Layout.addAfter Layout.Horizontal nexts
            & (`Layout.hoverInPlaceOf` evalView)
            & return

annotationSpacer :: Monad m => ExprGuiM m (Layout a)
annotationSpacer = ExprGuiM.vspacer Config.valAnnotationSpacing <&> Layout.fromCenteredWidget

addAnnotationH ::
    Monad m =>
    (AnimId -> ExprGuiM m (Layout (T f Widget.EventResult))) ->
    WideAnnotationBehavior -> Sugar.EntityId ->
    ExprGuiM m (ExpressionGui f -> ExpressionGui f)
addAnnotationH f wideBehavior entityId =
    do
        vspace <- annotationSpacer
        annotationLayout <- f animId
        processAnn <- processAnnotationGui animId wideBehavior
        return $
            \(ExpressionGui mkLayout) ->
            ExpressionGui $ \lp ->
            let layout = mkLayout lp
            in  layout
                & Layout.addAfter Layout.Vertical
                [ vspace
                , processAnn (layout ^. Widget.width) annotationLayout
                     & Layout.alignment . _1 .~ layout ^. Layout.alignment . _1
                ]
    where
        animId = WidgetIds.fromEntityId entityId & Widget.toAnimId

addInferredType ::
    Monad m =>
    Type -> WideAnnotationBehavior -> Sugar.EntityId ->
    ExprGuiM m (ExpressionGui f -> ExpressionGui f)
addInferredType typ = addAnnotationH (makeTypeView typ)

addEvaluationResult ::
    Monad m =>
    NeighborVals (Maybe EvalResDisplay) -> EvalResDisplay ->
    WideAnnotationBehavior -> Sugar.EntityId ->
    ExprGuiM m (ExpressionGui f -> ExpressionGui f)
-- REVIEW(Eyal): This is misleading when it refers to Previous results
addEvaluationResult neigh resDisp wideBehavior entityId =
    case (erdVal resDisp ^. ER.payload, erdVal resDisp ^. ER.body) of
    (T.TRecord T.CEmpty, _) ->
        addValBGWithColor Config.evaluatedPathBGColor (WidgetIds.fromEntityId entityId)
        <&> (egWidget %~)
    (_, ER.RFunc{}) -> return id
    _ -> addAnnotationH (makeEvalView neigh resDisp) wideBehavior entityId

parentExprFDConfig :: Config -> FocusDelegator.Config
parentExprFDConfig config = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = Config.enterSubexpressionKeys config
    , FocusDelegator.focusChildDoc = E.Doc ["Navigation", "Enter subexpression"]
    , FocusDelegator.focusParentKeys = Config.leaveSubexpressionKeys config
    , FocusDelegator.focusParentDoc = E.Doc ["Navigation", "Leave subexpression"]
    }

disallowedNameChars :: String
disallowedNameChars = "[]\\`()"

nameEditFDConfig :: FocusDelegator.Config
nameEditFDConfig = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = [ModKey mempty GLFW.Key'Enter]
    , FocusDelegator.focusChildDoc = E.Doc ["Edit", "Rename"]
    , FocusDelegator.focusParentKeys = [ModKey mempty GLFW.Key'Escape]
    , FocusDelegator.focusParentDoc = E.Doc ["Edit", "Done renaming"]
    }

deletionDiagonal ::
    Widget.R -> AnimId -> Anchors.DefinitionState -> ExpressionGui m -> ExpressionGui m
deletionDiagonal _ _ Anchors.LiveDefinition eg = eg
deletionDiagonal thickness animId Anchors.DeletedDefinition eg =
    eg
    & egWidget . Widget.view %~ f
    where
        f view =
            View.addDiagonal thickness (animId ++ ["diagonal"])
            (minLayer - 1) (Draw.Color 1 0 0 1) view
            where
                minLayer =
                    Lens.minimumOf (View.animFrame . Anim.layers) view
                    & fromMaybe 0

makeNameOriginEdit ::
    Monad m => Name m -> Widget.Id -> ExprGuiM m (Widget (T m Widget.EventResult))
makeNameOriginEdit name myId =
    do
        style <- ExprGuiM.readStyle
        let textEditStyle =
                case nNameSource name of
                NameSourceAutoGenerated -> Style.styleAutoNameOrigin style
                NameSourceStored -> Style.styleNameOrigin style
        makeNameEdit name myId -- myId goes directly to name edit
            & ExprGuiM.localEnv (WE.envTextStyle .~ textEditStyle)

makeNameEdit ::
    Monad m => Name m -> Widget.Id -> ExprGuiM m (Widget (T m Widget.EventResult))
makeNameEdit = makeNameEditWith id

makeNameEditWith ::
    Monad m =>
    (Widget (T m Widget.EventResult) -> Widget (T m Widget.EventResult)) ->
    Name m -> Widget.Id -> ExprGuiM m (Widget (T m Widget.EventResult))
makeNameEditWith onActiveEditor (Name nameSrc nameCollision setName name) myId =
    ExprGuiM.makeFocusDelegator nameEditFDConfig
    FocusDelegator.FocusEntryParent myId
    <*>
    do
        collisionSuffixes <-
            makeCollisionSuffixLabels nameCollision (Widget.toAnimId myId)
        nameEdit <-
            makeWordEdit (Property storedName setName) (WidgetIds.nameEditOf myId)
            & WE.localEnv emptyStringEnv
            & ExprGuiM.widgetEnv
        return . Box.hboxCentered $ nameEdit : collisionSuffixes
    <&> onActiveEditor
    where
        emptyStringEnv env = env
            & WE.envTextStyle . TextEdit.sEmptyFocusedString .~ ""
            & WE.envTextStyle . TextEdit.sEmptyUnfocusedString .~ name
        storedName =
            case nameSrc of
            NameSourceAutoGenerated -> ""
            NameSourceStored -> name
        makeWordEdit =
            BWidgets.makeWordEdit
            <&> Lens.mapped . Lens.mapped . Widget.eventMap
                %~ E.filterChars (`notElem` disallowedNameChars)

stdWrap ::
    Monad m =>
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m -> ExpressionGui m)
stdWrap pl =
    do
        exprEventMap <- ExprEventMap.make pl
        maybeAddAnnotationPl pl
            <&>
            \maybeAdd gui ->
            maybeAdd gui
            & egWidget %~ Widget.weakerEvents exprEventMap

makeFocusDelegator ::
    (Monad m, Monad f) =>
    FocusDelegator.Config ->
    FocusDelegator.FocusEntryTarget ->
    Widget.Id ->
    ExprGuiM m (ExpressionGui f -> ExpressionGui f)
makeFocusDelegator =
    ExprGuiM.makeFocusDelegator
    <&> Lens.mapped . Lens.mapped . Lens.mapped %~ (egWidget %~)

parentDelegator ::
    (Monad f, Monad m) => Widget.Id ->
    ExprGuiM m (ExpressionGui f -> ExpressionGui f)
parentDelegator myId =
    do
        config <- ExprGuiM.readConfig
        makeFocusDelegator (parentExprFDConfig config)
            FocusDelegator.FocusEntryChild (WidgetIds.notDelegatingId myId)

stdWrapParentExpr ::
    Monad m =>
    Sugar.Payload m ExprGuiT.Payload ->
    (Widget.Id -> ExprGuiM m (ExpressionGui m)) ->
    ExprGuiM m (ExpressionGui m)
stdWrapParentExpr pl mkGui =
    stdWrap pl <*> (parentDelegator myId <*> mkGui innerId)
    & ExprGuiM.assignCursor myId innerId
    where
        myId = WidgetIds.fromExprPayload pl
        innerId = WidgetIds.delegatingId myId

makeFocusableView ::
    (Applicative f, Monad m) =>
    Widget.Id ->
    ExprGuiM m
    ( Layout (f Widget.EventResult) ->
      Layout (f Widget.EventResult)
    )
makeFocusableView myId = ExprGuiM.widgetEnv (BWidgets.makeFocusableView myId)

makeLabel :: Monad m => String -> AnimId -> ExprGuiM m (Layout a)
makeLabel text animId =
    ExprGuiM.makeLabel text animId <&> Layout.fromCenteredWidget

grammarLabel :: Monad m => String -> AnimId -> ExprGuiM m (Layout f)
grammarLabel text animId =
    do
        config <- ExprGuiM.readConfig
        makeLabel text animId
            & ExprGuiM.localEnv (WE.setTextColor (Config.grammarColor config))

addValBG :: Monad m => Widget.Id -> ExprGuiM m (Widget f -> Widget f)
addValBG myId = addValBGWithColor Config.valFrameBGColor myId

addValBGWithColor ::
    (GTraversable t, Monad m) =>
    (Config -> Draw.Color) -> Widget.Id -> ExprGuiM m (WidgetF t a -> WidgetF t a)
addValBGWithColor color myId =
    do
        config <- ExprGuiM.readConfig
        let layer = Config.layerValFrameBG $ Config.layers config
        Widget.backgroundColor layer animId (color config) & return
    where
        animId = Widget.toAnimId myId ++ ["val"]

addValPadding :: Monad m => ExprGuiM m (ExpressionGui n -> ExpressionGui n)
addValPadding =
    ExprGuiM.readConfig <&> Config.valFramePadding <&> fmap realToFrac <&> pad

liftLayers :: Monad m => ExprGuiM m (Layout a -> Layout a)
liftLayers = ExprGuiM.widgetEnv BWidgets.liftLayerInterval

addValFrame ::
    Monad m => Widget.Id -> ExprGuiM m (ExpressionGui f -> ExpressionGui f)
addValFrame myId =
    (.)
    <$> (addValBG myId <&> (egWidget %~))
    <*> addValPadding

-- TODO: This doesn't belong here
makeNameView ::
    (Monad m, Monad n) =>
    Name n -> AnimId -> ExprGuiM m (Widget f)
makeNameView (Name _ collision _ name) animId =
    do
        label <- BWidgets.makeLabel name animId & ExprGuiM.widgetEnv
        suffixLabels <- makeCollisionSuffixLabels collision $ animId ++ ["suffix"]
        Box.hboxCentered (label : suffixLabels) & return

-- TODO: This doesn't belong here
makeCollisionSuffixLabels ::
    Monad m => NameCollision -> AnimId -> ExprGuiM m [Widget f]
makeCollisionSuffixLabels NoCollision _ = return []
makeCollisionSuffixLabels (Collision suffix) animId =
    do
        config <- ExprGuiM.readConfig
        let Config.Name{..} = Config.name config
            onSuffixWidget =
                Widget.backgroundColor (Config.layerNameCollisionBG (Config.layers config))
                    animId collisionSuffixBGColor .
                Widget.scale (realToFrac <$> collisionSuffixScaleFactor)
        BWidgets.makeLabel (show suffix) animId
            & WE.localEnv (WE.setTextColor collisionSuffixTextColor)
            <&> (:[]) . onSuffixWidget
            & ExprGuiM.widgetEnv

maybeAddAnnotationPl ::
    Monad m =>
    Sugar.Payload x ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui f -> ExpressionGui f)
maybeAddAnnotationPl pl =
    do
        wideAnnotationBehavior <-
            if showAnnotation ^. ExprGuiT.showExpanded
            then return KeepWideAnnotation
            else ExprGuiM.isExprSelected pl <&> wideAnnotationBehaviorFromSelected
        maybeAddAnnotation wideAnnotationBehavior
            showAnnotation
            (pl ^. Sugar.plAnnotation)
            (pl ^. Sugar.plEntityId)
    where
        showAnnotation = pl ^. Sugar.plData . ExprGuiT.plShowAnnotation

evaluationResult ::
    Monad m =>
    Sugar.Payload m ExprGuiT.Payload -> ExprGuiM m (Maybe (ER.Val Type))
evaluationResult pl =
    ExprGuiM.readMScopeId
    <&> valOfScope (pl ^. Sugar.plAnnotation)
    <&> Lens.mapped %~ erdVal

data EvalAnnotationOptions
    = NormalEvalAnnotation
    | WithNeighbouringEvalAnnotations (NeighborVals (Maybe Sugar.BinderParamScopeId))

maybeAddAnnotation ::
    Monad m =>
    WideAnnotationBehavior -> ShowAnnotation -> Sugar.Annotation -> Sugar.EntityId ->
    ExprGuiM m (ExpressionGui f -> ExpressionGui f)
maybeAddAnnotation = maybeAddAnnotationWith NormalEvalAnnotation

data AnnotationMode
    = AnnotationModeNone
    | AnnotationModeTypes
    | AnnotationModeEvaluation (NeighborVals (Maybe EvalResDisplay)) EvalResDisplay

getAnnotationMode :: Monad m => EvalAnnotationOptions -> Sugar.Annotation -> ExprGuiM m AnnotationMode
getAnnotationMode opt annotation =
    do
        settings <- ExprGuiM.readSettings
        case settings ^. CESettings.sInfoMode of
            CESettings.None -> return AnnotationModeNone
            CESettings.Types -> return AnnotationModeTypes
            CESettings.Evaluation ->
                ExprGuiM.readMScopeId <&> valOfScope annotation
                <&> maybe AnnotationModeNone (AnnotationModeEvaluation neighbourVals)
    where
        neighbourVals =
            case opt of
            NormalEvalAnnotation -> NeighborVals Nothing Nothing
            WithNeighbouringEvalAnnotations neighbors ->
                neighbors <&> (>>= valOfScopePreferCur annotation . (^. Sugar.bParamScopeId))

maybeAddAnnotationWith ::
    Monad m =>
    EvalAnnotationOptions -> WideAnnotationBehavior -> ShowAnnotation ->
    Sugar.Annotation -> Sugar.EntityId ->
    ExprGuiM m (ExpressionGui f -> ExpressionGui f)
maybeAddAnnotationWith opt wideAnnotationBehavior ShowAnnotation{..} annotation entityId =
    getAnnotationMode opt annotation
    >>= \case
    AnnotationModeNone
        | _showExpanded -> withType
        | otherwise -> noAnnotation
    AnnotationModeEvaluation n v ->
        case _showInEvalMode of
        EvalModeShowNothing -> noAnnotation
        EvalModeShowType -> withType
        EvalModeShowEval -> withVal n v
    AnnotationModeTypes
        | _showInTypeMode -> withType
        | otherwise -> noAnnotation
    where
        noAnnotation = pure id
        -- concise mode and eval mode with no result
        inferredType = annotation ^. Sugar.aInferredType
        withType =
            addInferredType inferredType wideAnnotationBehavior entityId
        withVal neighborVals scopeAndVal =
            addEvaluationResult neighborVals scopeAndVal
            wideAnnotationBehavior entityId

valOfScope :: Sugar.Annotation -> CurAndPrev (Maybe ER.ScopeId) -> Maybe EvalResDisplay
valOfScope annotation mScopeIds =
    go
    <$> curPrevTag
    <*> annotation ^. Sugar.aMEvaluationResult
    <*> mScopeIds
    & fallbackToPrev
    where
        go _ _ Nothing = Nothing
        go tag ann (Just scopeId) =
            ann ^? Lens._Just . Lens.at scopeId . Lens._Just
            <&> EvalResDisplay scopeId tag

valOfScopePreferCur :: Sugar.Annotation -> ER.ScopeId -> Maybe EvalResDisplay
valOfScopePreferCur annotation = valOfScope annotation . pure . Just

listWithDelDests :: k -> k -> (a -> k) -> [a] -> [(k, k, a)]
listWithDelDests = ListUtils.withPrevNext
