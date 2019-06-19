{-|

   File: xmonad.hs
   Description: Short and sweet xmonad configuration (which is still in its infancy)
   Author: davidm19
   Date: June 18th, 2019
 
-}

-- Section: Imports
-- Base Imports
import XMonad
import System.Exit (exitSuccess)
import XMonad.Config.Desktop
import qualified XMonad.StackSet as W

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad

-- Hooks
import XMonad.Hooks.DynamicLog

-- Layouts
import XMonad.Layout.Dwindle
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.SimplestFloat
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

-- Section: Main Function and Configuration
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- myTerminal = "xfce4-terminal --hide-menubar --hide-scrollbar --hide-borders"
-- This is also a safe option if st is being too finicky. Keep in mind, however, only st
-- is able to be used as a scratchpad, so you'll have to change this in the scratchpad
-- section.
myTerminal = "st"

myConfig = desktopConfig
     { layoutHook = myLayout
     , terminal   = myTerminal
     } `removeKeys` [ (mod1Mask, xK_b) ]
     `additionalKeysP`         myKeys

-- Section: XMobar Configuration
myBar = "xmobar"
myPP  = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (mod4Mask .|. controlMask, xK_b)

-- Section: Layouts
myLayout = smartBorders
         $ mkToggle (NOBORDERS ?? FULL ?? EOT)
         $ Dwindle R CW (3/2) (11/10) ||| simplestFloat ||| Full

-- Section: Keybindings
myKeys =
        -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")
        , ("M-S-r", spawn "xmonad --restart")
        , ("M-S-q", io exitSuccess)
        , ("M-p",   spawn "rofi -show run")

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
    spawnTerm  = myTerminal ++  " -n scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
                 where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
