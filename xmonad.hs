------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
-- validate syntax: xmonad --recompile
{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------

import XMonad hiding (Tall)
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.ComboP
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.Grid
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run
import Control.Monad (liftM2)
import Data.Monoid
import Graphics.X11
import Graphics.X11.Xinerama
import System.Exit
import System.IO

import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- XMonad:
main = do
  dzen <- spawnPipe myStatusBar
  conky <- spawnPipe conkyBar
  xbindkeys <- spawnPipe "xbindkeys"
  xmonad $ myUrgencyHook $ defaultConfig

    { terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor

    , mouseBindings      = myMouseBindings

    , layoutHook         = myLayout
    , manageHook         = myManageHook <+> manageDocks <+> dynamicMasterHook
    , handleEventHook    = myEventHook
    , logHook            = dynamicLogWithPP $ myDzenPP dzen
    , startupHook        = myStartupHook
    }

myTerminal = "gnome-terminal"
myFocusFollowsMouse = True
myBorderWidth = 1
myModMask = mod4Mask
myWorkspaces = ["1:devel", "2:www", "3:compile", "4:misc", "5:vbox", "6:6", "7:7", "8:8", "9:9"]
myNormalBorderColor = "#0f0f0f"
myFocusedBorderColor = "#1f1f1f"

myEventHook = mempty
myStartupHook = return ()

-- Color, font and iconpath definitions:
myFont = "-*-montecarlo-medium-r-normal-*-11-*-*-*-c-*-*-*"
myIconDir = "/home/jespino/.xmonad/dzen"
myDzenFGColor = "#555555"
myDzenBGColor = "#222222"
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"
myFocusedFGColor = "#f0f0f0"
myFocusedBGColor = "#333333"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#0077ff"
myIconFGColor = "#777777"
myIconBGColor = "#0f0f0f"
myPatternColor = "#1f1f1f"
mySeperatorColor = "#555555"

-- GSConfig options:
myGSConfig = defaultGSConfig
    { gs_cellheight = 50
    , gs_cellwidth = 250
    , gs_cellpadding = 10
    , gs_font = "" ++ myFont ++ ""
    }

-- XPConfig options:
myXPConfig = defaultXPConfig
    { font = "" ++ myFont ++ ""
    , bgColor = "" ++ myNormalBGColor ++ ""
    , fgColor = "" ++ myNormalFGColor ++ ""
    , fgHLight = "" ++ myNormalFGColor ++ ""
    , bgHLight = "" ++ myUrgentBGColor ++ ""
    , borderColor = "" ++ myFocusedBorderColor ++ ""
    , promptBorderWidth = 1
    , position = Bottom
    , height = 16
    , historySize = 100
    }

-- Theme options:
myTheme = defaultTheme
    { activeColor = "" ++ myFocusedBGColor ++ ""
    , inactiveColor = "" ++ myDzenBGColor ++ ""
    , urgentColor = "" ++ myUrgentBGColor ++ ""
    , activeBorderColor = "" ++ myFocusedBorderColor ++ ""
    , inactiveBorderColor = "" ++ myNormalBorderColor ++ ""
    , urgentBorderColor = "" ++ myNormalBorderColor ++ ""
    , activeTextColor = "" ++ myFocusedFGColor ++ ""
    , inactiveTextColor = "" ++ myDzenFGColor ++ ""
    , urgentTextColor = "" ++ myUrgentFGColor ++ ""
    , fontName = "" ++ myFont ++ ""
    }

-- Statusbar options:
myStatusBar = "dzen2 -x '0' -y '0' -h '16' -w '800' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
conkyBar = "conky | dzen2 -x '800' -y '0' -h '16' -ta 'r' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"

-- Urgency hint options:
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-x", "0", "-y", "1184", "-h", "16", "-w", "1920", "-ta", "r", "-expand", "l", "-fg", "" ++ myUrgentFGColor ++ "", "-bg", "" ++ myNormalBGColor ++ "", "-fn", "" ++ myFont ++ ""] }

-- Layouts:
myLayout = avoidStruts $ layoutHints $ onWorkspace "3:compile" (Grid) $ smartBorders (Full ||| resizableTile ||| Mirror resizableTile ||| tabbedLayout ||| Grid)
    where
    resizableTile = ResizableTall nmaster delta ratio []
    tabbedLayout = tabbedAlways shrinkText myTheme
    nmaster = 1
    ratio = toRational (2/(1+sqrt(5)::Double))
    delta = 3/100

-- Mouse bindings:
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) -- set the window to floating mode and move by dragging
    , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster)) -- raise the window to the top of the stack
    , ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) -- set the window to floating mode and resize by dragging
    , ((modMask, button4), (\_ -> prevWS)) -- switch to previous workspace
    , ((modMask, button5), (\_ -> nextWS)) -- switch to next workspace
    ]

-- Window rules:
myManageHook = composeAll . concat $
    [ [isDialog --> doFloat]
    , [className =? c --> doFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "1:devel" | x <- my1Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "2:www" | x <- my2Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "3:compile" | x <- my3Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "4:misc" | x <- my4Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "5:vbox" | x <- my5Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "6:6" | x <- my6Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "7:7" | x <- my7Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "8:8" | x <- my8Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "9:9" | x <- my9Shifts]
    ]
    where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["MPlayer", "Skype", "Xmessage"]
    myTFloats = []
    myRFloats = []
    myIgnores = ["desktop_window", "kdesktop"]
    my1Shifts = []
    my2Shifts = ["Chromium"]
    my3Shifts = []
    my4Shifts = ["Eog", "Evince", "MPlayer"]
    my5Shifts = ["VirtualBox"]
    my6Shifts = []
    my7Shifts = []
    my8Shifts = []
    my9Shifts = []

-- dynamicLog pretty printer for dzen:
myDzenPP h = defaultPP
    { ppCurrent = wrap ("^p(2)^ib(1)^fg(" ++ myFocusedBGColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()O^fg(" ++ myNormalFGColor ++ ")^p(2)") ("^p(2)^fg(" ++ myFocusedBGColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()^bg()^p()") . \wsId -> dropIx wsId
    , ppVisible = wrap ("^p(2)^ib(1)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()O^fg(" ++ myNormalFGColor ++ ")^p(2)") ("^p(2)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()^bg()^p()") . \wsId -> dropIx wsId
    , ppHidden = wrap ("^p(2)^ib(1)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^fg()^bg()^p()O^p(2)") ("^p(2)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^p()^ib(0)^fg()^bg()^p()") . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId -- don't use ^fg() here!!
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else wrap ("^p(2)^ib(1)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^fg(" ++ myDzenFGColor ++ ")^bg()^p()O^p(2)") ("^p(2)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()^bg()^p()") . dropIx $ wsId
    , ppUrgent = wrap (("^p(2)^ib(1)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p()O^fg(" ++ myUrgentFGColor ++ ")^p(2)")) ("^p(2)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()^bg()^p()") . \wsId -> dropIx wsId
    , ppSep = " "
    , ppWsSep = ""
    , ppTitle = dzenColor ("" ++ myNormalFGColor ++ "") "" . wrap ("^ib(1)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^p(2)^fg()< ") (" >^p(2)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()")
    , ppLayout = dzenColor ("" ++ myNormalFGColor ++ "") "" .
        (\x -> case x of
        "Hinted Full" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-full.xbm)"
        "Hinted ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-tall-right.xbm)"
        "Hinted Mirror ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-mirror-bottom.xbm)"
        "Hinted Tabbed Simplest" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-tabbed.xbm)"
        "Hinted Grid" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-grid.xbm)"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
    dropIx wsId = if (':' `elem` wsId) then drop 2 wsId else wsId
    staticWs = ["1:devel", "2:www", "3:compile", "4:misc", "5:vbox"]
