import XMonad
import System.Exit (exitSuccess)
import XMonad.Config.Desktop
import XMonad.Layout.Dwindle
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Hooks.DynamicLog
import XMonad.Layout.SimplestFloat
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Util.EZConfig (additionalKeysP)
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

-- Main Function
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Main Configuration
myTerminal = "st"

myConfig = desktopConfig
     { layoutHook = myLayout
     , terminal   = myTerminal
     } `additionalKeysP`         myKeys

-- XMobar Configuration
myBar = "xmobar"
myPP  = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Layouts
myLayout = smartBorders
         $ mkToggle (NOBORDERS ?? FULL ?? EOT)
         $ Dwindle R CW (3/2) (11/10) ||| simplestFloat ||| Full

-- Keybindings
myKeys =
        -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")      -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")        -- Restarts xmonad
        , ("M-S-q", io exitSuccess)                  -- Quits xmonad

        -- Scratchpads
        , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")

        -- Layouts
        , ("M-<Space>", sendMessage NextLayout)      -- Switch to next layout
        , ("M-S-f", sendMessage (T.Toggle "simpleFloat"))
        , ("M-S-m", sendMessage $ Toggle FULL ) ]

-- Scratchpads
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

