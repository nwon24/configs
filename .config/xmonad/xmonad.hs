import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig
import XMonad.Util.Loggers

myTerminal :: String
myTerminal = "st"

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)) toggleStrutsKey
     $ myConfig
  where
    -- Toggle showing the bar using M-S-b
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m .|. shiftMask, xK_b)

myConfig = def
    { modMask    = mod4Mask  -- Rebind Mod to the Suer key
    , terminal   = myTerminal -- Change terminal emulator from default (xterm)
    , layoutHook = myLayout  -- Use custom layouts
    }
   `additionalKeysP`
    [ ("M-b"                    , spawn "firefox"                     )
    , ("<XF86AudioMute>"        , spawn "amixer set Master toggle"    )
    , ("<XF86AudioRaiseVolume>" , spawn "amixer set Master 5%+ unmute")
    , ("<XF86AudioLowerVolume>" , spawn "amixer set Master 5%- unmute")
    ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment when resizing

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             =  " : "                           -- Separator character between output
    , ppTitle           = shorten 30                       -- Shorten the title of the window to 30 chars
    , ppCurrent         = white . xmobarBorder "Top" "#8be9fd" 2   -- Place a coloured bar on top of current workspace
    , ppHidden          = white                            -- Colour of workspaces that have windows in them
    , ppHiddenNoWindows = lowWhite                         -- Colour of workspaces with no windows
    }
  -- Just some colours in case we need them
  where
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta   = xmobarColor "#ff79c6" ""
    blue      = xmobarColor "#bd93f9" ""
    white     = xmobarColor "#f8f8f2" ""
    yellow    = xmobarColor "#f1fa8c" ""
    red       = xmobarColor "#ff5555" ""
    lowWhite  = xmobarColor "#bbbbbb" ""
