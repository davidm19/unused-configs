{-|

   File: xmonad.hs
   Description: Short and sweet xmonad configuration (which is still in its infancy)
   Author: davidm19
   Date: June 18th, 2019
 
-}

-- Section: Imports

-- Base Imports
import XMonad
import XMonad.Config.Desktop
import qualified XMonad.StackSet as W
import System.Exit (exitSuccess)

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP, removeKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

-- Layouts
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

-- Section: Main Function and Configuration
-- main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
main = do
    xmproc <- spawnPipe "xmobar ~/.xmobar/xmobar.hs"
    xmonad $ desktopConfig
         { layoutHook = myLayout
         , manageHook = manageDocks <+> manageHook desktopConfig
         , terminal   = myTerminal
         , logHook    = dynamicLogWithPP $ defaultPP
             { ppOutput  = hPutStrLn xmproc
	     , ppTitle   = xmobarColor "green" "" . shorten 50
	     , ppCurrent = xmobarColor "#6bf7ff" "" . wrap "<" ">"
	     }
         } `removeKeys` [ (mod1Mask, xK_b) ]
         `additionalKeysP`         myKeys

-- myTerminal = "xfce4-terminal --hide-menubar --hide-scrollbar --hide-borders"
-- This is also a safe option if st is being too finicky. Keep in mind, however, only st
-- is able to be used as a scratchpad, so you'll have to change this in the scratchpad
-- section.
myTerminal = "st"

-- Section: Layouts
myLayout = smartBorders $ avoidStruts $ mkToggle (NOBORDERS ?? FULL ?? EOT)
         $ ResizableTall 1 (3/100) (1/2) [] ||| TwoPane (3/100) (1/2) ||| simplestFloat ||| noBorders (tabbed shrinkText myTabConfig)

myTabConfig = def { activeColor         = "#353535"
                  , inactiveColor       = "#000000"
                  , urgentColor         = "#FF0000"
                  , activeBorderColor   = "#FF0000"
                  , inactiveBorderColor = "#000000"
                  , urgentBorderColor   = "#0087FF"
                  , activeTextColor     = "#FFFFFF"
                  , inactiveTextColor   = "#FFFFFF"
                  , urgentTextColor     = "#FFF200"
                  , fontName            = "xft:curie:size = 11:antialias = true"
                  }

-- Section: Keybindings
myKeys =
        -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")
        , ("M-S-r", spawn "xmonad --restart")
        , ("M-S-q", io exitSuccess)
        , ("M-p",   spawn "rofi -show run")
        , ("M-S-p",   spawn "bash $HOME/power.sh")

        -- Scratchpads
        , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")

        -- Layouts
        , ("M-<Space>", sendMessage NextLayout)
        , ("M-S-f", sendMessage (T.Toggle "simpleFloat"))
        , ("M-S-m", sendMessage $ Toggle FULL ) ]

-- Section: Scratchpads
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                ]
    where
    spawnTerm  = myTerminal ++ " -n scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
                 where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
