import XMonad
import Data.Monoid
import System.Exit

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce
import XMonad.Util.Loggers
import XMonad.Util.ClickableWorkspaces

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks


import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- XMobar imports
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

-- Extending
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CycleWS

import XMonad.Layout.NoBorders

main :: IO ()
main = xmonad 
     . ewmhFullscreen 
     . ewmh 
     . withEasySB(statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (clickablePP myXmobarPP)) defToggleStrutsKey
     $ docks myConfig

-- My Configuration -----------------------------------------------------------------------------
myConfig = def 
  { modMask             = mod4Mask
  , terminal            = "alacritty"
  , workspaces          = ["1","2","3","4","5","6","7","8","9"]
  , focusFollowsMouse   = False
  
  , borderWidth         = 2
  , normalBorderColor   = "#333333"
  , focusedBorderColor  = "#cc8833"
  
  , layoutHook          = myLayouts
  , manageHook          = myManageHook
  , handleEventHook     = myHandleEvenHook
  , logHook             = myLogHook
  , startupHook         = myStartupHook
  }

-- Keybinds -------------------------------------------------------------------------------------
  `additionalKeys`
    [ 
    -- Windows
      ((mod4Mask, xK_q),   kill) 
    , ((mod4Mask .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Workspaces
    , ((mod4Mask, xK_Tab), toggleRecentWS)
    , ((mod4Mask, xK_h),   prevWS)
    , ((mod4Mask, xK_l),   nextWS)

    -- Layouts
    , ((mod4Mask, xK_b), sendMessage ToggleStruts)

    -- Apps
    , ((mod4Mask, xK_Return), spawn "alacritty")
    , ((mod4Mask, xK_e),      spawn "dolphin")
    , ((mod4Mask, xK_p),      spawn "rofi -modi drun -show drun -theme ~/.config/rofi/themes/my_dracula.rasi")

    -- Audio/Volume
    , ((mod4Mask, xK_minus), spawn "pamixer --decrease 5")
    , ((mod4Mask, xK_equal), spawn "pamixer --increase 5")
    , ((mod4Mask, xK_0),     spawn "pamixer --toggle-mute")
    , ((mod4Mask, xK_F9),    spawn "/home/pedro/programming/dotfiles/scripts/change-default-sink.sh")

    -- Session managment
    , ((mod4Mask .|. shiftMask, xK_z),  spawn "i3lock -i /home/pedro/media/images/wallpaper/lock.png -u")
    , ((mod4Mask .|. shiftMask, xK_r),  spawn "xmonad --recompile; xmonad --restart")
    , ((mod4Mask .|. shiftMask, xK_F3), spawn "systemctl suspend")
    , ((mod4Mask .|. shiftMask, xK_F4), io (exitWith ExitSuccess))
    ]

-- LayoutHook -----------------------------------------------------------------------------------
myLayouts = avoidStruts( smartBorders( tiled ||| Mirror tiled ||| Full ) )
  where
    nmaster = 1      -- Default number of windows in the master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    tiled   = Tall nmaster delta ratio

-- Manage hook ----------------------------------------------------------------------------------
-- use $ xprop | grep WM_CLASS then click to get the name to type
myManageHook = composeAll
  [ className =? "mpv"  --> doFloat
  , className =? "Gimp" --> doFloat
  , isDialog            --> doFloat
  ]

-- Handle event hook ----------------------------------------------------------------------------
myHandleEvenHook = docksEventHook
-- myHandleEvenHook = fullscreenEventHook

-- Log hook -------------------------------------------------------------------------------------
myLogHook = return ()

-- Startup hook ---------------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "trayer --edge top --align right --SetPartialStrut true --width 10 --tint 0x212121 --height 21 --alpha 0"

-- XMobar ---------------------------------------------------------------------------------------
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = white " | "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = white . wrap (white " [") (white "]")
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = gray . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[ ") (white    " ]") . white . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . gray    . ppWindow

    -- Windows should have *some* title, which should not not exceed a sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
    gray     = xmobarColor "#656565" ""
