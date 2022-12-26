-- #############################################################################
-- #
-- # Pedro Frohmut
-- # github.com/pedrofrohmut/dotfiles/xmonad
-- #
-- ### CUSTOM_SCRIPTS ##########################################################
-- # audio-commands
-- # power-commands
-- # rofi-power
-- ### APP_DEPENDENCIES ########################################################
-- # thunar / pcmanfm
-- # firefox / brave
-- # deadbeef
-- # rofi
-- #############################################################################

import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- # Utils ---------------------------------------------------------------------
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Loggers
import XMonad.Util.ClickableWorkspaces (clickablePP)

-- # Hooks ---------------------------------------------------------------------
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.InsertPosition

-- # Actions -------------------------------------------------------------------
import XMonad.Actions.CycleRecentWS (toggleRecentWS)
import XMonad.Actions.CycleWS (prevWS, nextWS)

-- # Layout --------------------------------------------------------------------
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ResizableTile

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB
         (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (clickablePP myXmobarPP))
         defToggleStrutsKey
     . docks
     $ myConfig

-- My Configuration ------------------------------------------------------------
myConfig = def
    { modMask             = mod4Mask
    , terminal            = "alacritty"
    , workspaces          = ["1","2","3","4","5","6","7","8","9"]
    , focusFollowsMouse   = False

    , borderWidth         = 2
    , normalBorderColor   = "#343434"
    , focusedBorderColor  = "#cc8833"

    , layoutHook          = myLayouts
    , manageHook          = myManageHook
    , handleEventHook     = myHandleEvenHook
    , logHook             = myLogHook
    , startupHook         = myStartupHook
    }

-- Keybinds --------------------------------------------------------------------
  -- As the name suggest just additional keys, all the rest refer to defaults
  `additionalKeysP`
    -- Windows -----------------------------------------------------------------
      -- kill focused window
    [ ("M-q", kill)
      -- swap focused with master
    , ("M-S-m", windows W.swapMaster)
      -- Shrink Master Stack
    , ("M-C-h", sendMessage Shrink)
      -- Expand Master Stack
    , ("M-C-l", sendMessage Expand)

    -- Workspaces --------------------------------------------------------------
      -- Go to previous workspaces (or last when in the first)
    , ("M-h", prevWS)
      -- Go to next workspaces (or first when in the last)
    , ("M-l", nextWS)
      -- Go back and forth between 2 workspaces
    , ("M-<Tab>", toggleRecentWS)

    -- Layouts -----------------------------------------------------------------
      -- Toggle statusbar (XMobar)
    , ("M-b", sendMessage ToggleStruts)

    -- Apps (change it to fit your apss) ---------------------------------------
      -- Terminal emulator
    , ("M-<Return>", spawn "alacritty")
      -- File manager
    , ("M-e", spawn "pcmanfm")
      -- Web Browser
    , ("M-w", spawn "firefox")
      -- Emacs client
    , ("M-a", spawn "emacs")
      -- AppFinder
    , ("M-p", spawn "rofi -modi drun -show drun -show-icons \
                    \ -theme ~/.config/rofi/themes/my_dracula.rasi")
      -- XFCE4 AppFinder
    , ("M-S-p", spawn "xfce4-appfinder")

    -- Audio/Volume ------------------------------------------------------------
      -- Decrease volume
    , ("M--",   spawn "audio-commands decrease")
      -- Increase volume
    , ("M-=",   spawn "audio-commands increase")
      -- Toggle Mute
    , ("M-S-0", spawn "audio-commands toggle-mute")
      -- Call my script change output device
    , ("M-0",   spawn "audio-commands change")

    -- Deadbeef (Ctrl + Alt for Music) --------------------------------------------
      -- Next track - random order
    , ("M1-C-l", spawn "deadbeef --random")
      -- Toggle pause
    , ("M1-C-k", spawn "deadbeef --toggle-pause")
      -- Previous track
    , ("M1-C-j", spawn "deadbeef --prev")
      -- Stop playing
    , ("M1-C-h", spawn "deadbeef --stop")
      -- Volume Up by 5
    , ("M1-C-=", spawn "deadbeef --volume +5")
      -- Volume Down by 5
    , ("M1-C--", spawn "deadbeef --volume -5")

    -- Session managment -------------------------------------------------------
      -- Lock screen and suspend
    , ("M-S-<F2>", spawn "power-commands lock-suspend")
      -- Non-locking suspend (fast way)
    , ("M-S-<F3>", spawn "power-commands suspend")
      -- Rofi power menu
    , ("M1-<F4>",  spawn "rofi-power")

    -- Xmonad Session ----------------------------------------------------------
      -- Recompile the config and restart the window manager
    , ("M-S-r",    spawn "xmonad --recompile; xmonad --restart")
      -- Kill then window manager. The same as 'killall xmonad'
    , ("M-C-<F4>", io (exitWith ExitSuccess))
    ]

-- LayoutHook ------------------------------------------------------------------
myLayouts = avoidStruts $ smartBorders $ tiled ||| Mirror tiled ||| Full
  where
    nmaster = 1      -- Default number of windows in the master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    tiled   = Tall nmaster delta ratio

-- Manage hook -----------------------------------------------------------------
-- use $ xprop | grep WM_CLASS then click to get the name to type rules for windows
-- insertPosition: change where the new window will appear
myManageHook :: ManageHook
myManageHook = composeOne
    [ checkDock                      -?> doIgnore -- equivalent to manageDocks
    , isDialog                       -?> doCenterFloat
    , className =? "Gimp"            -?> doCenterFloat
    , className =? "MPlayer"         -?> doCenterFloat
    , className =? "Galculator"      -?> doCenterFloat
    , className =? "Pamac-manager"   -?> doCenterFloat
    , className =? "Lxpolkit"        -?> doCenterFloat
    , className =? "Image Resizer"   -?> doCenterFloat
    , className =? "Xfce4-appfinder" -?> doCenterFloat
    , return True -?> doF W.swapDown
    ]

-- Handle event hook -----------------------------------------------------------
myHandleEvenHook = mempty

-- Log hook --------------------------------------------------------------------
myLogHook = return ()

-- Startup hook ----------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "bash ~/dotfiles/scripts/trayer-cmd.sh &"

-- XMobar ----------------------------------------------------------------------
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = gray " : "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = white . wrap (white " [") (white "]")
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = gray . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[ ") (white    " ]") . white . ppWindow
    formatUnfocused = wrap (lowWhite "[")  (lowWhite "]")  . gray  . ppWindow

    -- Windows should have *some* title, which should not not exceed a sane
    -- length.
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
