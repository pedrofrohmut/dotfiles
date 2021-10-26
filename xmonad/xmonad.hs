import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- # Utils ---------------------------------------------------------------------
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce
import XMonad.Util.Loggers
import XMonad.Util.ClickableWorkspaces

-- # Hooks ---------------------------------------------------------------------
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

-- # Actions -------------------------------------------------------------------
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CycleWS

-- # Layout --------------------------------------------------------------------
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
  `additionalKeysP`
    [ 
    -- Windows
      ("M-q",          kill) 
    , ("M-S-<Return>", windows W.swapMaster)

    -- Workspaces
    , ("M-<Tab>", toggleRecentWS)
    , ("M-h",     prevWS)
    , ("M-l",     nextWS)

    -- Layouts
    , ("M-b", sendMessage ToggleStruts)

    -- Apps
    , ("M-<Return>", spawn "alacritty")
    , ("M-e",        spawn "dolphin")
    , ("M-p",        spawn "rofi -modi drun -show drun \
                            \ -theme ~/.config/rofi/themes/my_dracula.rasi")

    -- Audio/Volume
    , ("M--",   spawn "pamixer --decrease 5")
    , ("M-=",   spawn "pamixer --increase 5")
    , ("M-S-0", spawn "pamixer --toggle-mute")
    , ("M-0",   spawn "/home/pedro/programming/dotfiles/scripts/change-default-sink.sh")

    -- Session managment
    , ("M-C-r",    spawn "xmonad --recompile")
    , ("M-S-r",    spawn "xmonad --recompile; xmonad --restart")
    , ("M-S-<F2>", spawn "i3lock -i /home/pedro/media/images/wallpaper/lock.png -u")
    , ("M-S-<F3>", spawn "systemctl suspend")
    , ("M-S-<F4>", io (exitWith ExitSuccess))
    ]

-- LayoutHook -----------------------------------------------------------------------------------
myLayouts = avoidStruts $ smartBorders $ tiled ||| Mirror tiled ||| Full
  where
    nmaster = 1      -- Default number of windows in the master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    tiled   = Tall nmaster delta ratio

-- Manage hook ----------------------------------------------------------------------------------
-- use $ xprop | grep WM_CLASS then click to get the name to type
myManageHook :: ManageHook
myManageHook = composeAll
  [ isDialog                        --> doCenterFloat 
  , className =?   "mpv"            --> doFloat
  , className =?   "Gimp"           --> doFloat
  , className =?   "pavucontrol"    --> doCenterFloat
  , title =?       "Downloads"      --> doFloat
  , title =?       "Save As..."     --> doFloat
  ]

-- Handle event hook ----------------------------------------------------------------------------
myHandleEvenHook = docksEventHook

-- Log hook -------------------------------------------------------------------------------------
myLogHook = return ()

-- Startup hook ---------------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "trayer --edge top --align right --SetPartialStrut true --width 10 \
               \ --tint 0x212121 --height 21 --alpha 0"

-- XMobar ---------------------------------------------------------------------------------------
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = gray " : "
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
    formatUnfocused = wrap (lowWhite "[")  (lowWhite "]")  . gray  . ppWindow

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
