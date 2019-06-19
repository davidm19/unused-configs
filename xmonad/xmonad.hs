import XMonad
import XMonad.Config.Desktop
import XMonad.Layout.Dwindle
import XMonad.Hooks.DynamicLog

-- Main Function
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Main Configuration
myConfig = desktopConfig
     { layoutHook = myLayout
     , terminal   = "st"
     }

-- XMobar Configuration
myBar = "xmobar"
myPP  = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Layouts
myLayout = Dwindle R CW (3/2) (11/10)
-- namedScratchpads (dwt1)

-- simpleFloat
-- Monocle
