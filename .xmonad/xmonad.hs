-- Simple XMonad config
-- Nothing fancy here.

import XMonad

import XMonad.Hooks.DynamicLog

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "st"

myWebBrowser :: String
myWebBrowser = "firefox"

main :: IO ()
main = xmonad =<< statusBar "xmobar ~/.config/xmobar/xmobarrc" myXmobarPP toggleStrutsKey myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m} = (m .|. shiftMask, xK_b)

myConfig = def
    { modMask = myModMask
    , terminal = myTerminal
    , layoutHook = myLayoutHook
    , startupHook = myStartupHook
    }
   `additionalKeysP`
    [ ("M-b", spawn myWebBrowser)
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
    ]

myLayoutHook = tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "xwallpaper --stretch ~/wallpaper &"

myXmobarPP :: PP
myXmobarPP = def
    { ppCurrent = yellow . wrap "[" "]"
    , ppVisible = yellow
    , ppHidden = blue
    , ppHiddenNoWindows = red
    , ppTitle = green . shorten 30
    , ppLayout = magenta
    }
  where
    yellow, blue, red, green, magenta :: String -> String
    yellow = xmobarColor "#f1fa8c" ""
    blue = xmobarColor "#8be9fd" ""
    red = xmobarColor "#ff5555" ""
    green = xmobarColor "#50fa7b" ""
    magenta = xmobarColor "#ff79c6" "" 
