-- Pedro Frohmut
-- github.com/pedrofrohmut

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
      -- Go back and forth between 2 workspaces
    , ("M-<Tab>", toggleRecentWS)  
      -- Go to previous workspaces (or last when in the first) 
    , ("M-h", prevWS)  
      -- Go to next workspaces (or first when in the last)
    , ("M-l", nextWS)  

    -- Layouts -----------------------------------------------------------------
      -- Toggle statusbar (XMobar)
    , ("M-b", sendMessage ToggleStruts)

    -- Apps (change it to fit your apss) ---------------------------------------
      -- Terminal emulator
    , ("M-<Return>", spawn "alacritty")
      -- File manager
    , ("M-e", spawn "thunar")  
      -- Web Browser
    , ("M-w", spawn "brave")  
      -- AppFinder
    , ("M-p", spawn "rofi -modi drun -show drun -show-icons \
                    \ -theme ~/.config/rofi/themes/my_dracula.rasi")  
      -- XFCE4 AppFinder
    , ("M-S-p", spawn "xfce4-appfinder")

    -- Audio/Volume ------------------------------------------------------------
      -- Decrease volume
    , ("M--", spawn "pamixer --decrease 5")
      -- Increase volume
    , ("M-=", spawn "pamixer --increase 5")
      -- Toggle Mute
    , ("M-S-0", spawn "pamixer --toggle-mute")
      -- Call my script change output device
    , ("M-0", spawn "/home/pedro/dotfiles/scripts/change-default-sink.sh")

    -- Deadbeef (Alt M - for Music) --------------------------------------------
      -- Next track - random order
    , ("M1-m M1-l", spawn "deadbeef --random")
      -- Toggle pause
    , ("M1-m M1-k", spawn "deadbeef --toggle-pause")
      -- Stop playing
    , ("M1-m M1-j", spawn "deadbeef --stop")
      -- Volume Up by 5
    , ("M1-m M1-=", spawn "deadbeef --volume +5")
      -- Volume Down by 5
    , ("M1-m M1--", spawn "deadbeef --volume -5")

    -- Session managment -------------------------------------------------------
      -- Recompile the config and restart the window manager
    , ("M-S-r",    spawn "xmonad --recompile; xmonad --restart")
      -- Lock screen with a lock image. And -u for not showing the feedback animation
    , ("M-S-<F2>", spawn "i3lock -i /home/pedro/media/images/wallpaper/lock.png -u")
      -- Non-locking suspend (fast way)
    , ("M-S-<F3>", spawn "systemctl suspend")
      -- Kill then window manager. The same as 'killall xmonad'
    , ("M-S-<F4>", io (exitWith ExitSuccess))
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
    [ checkDock              -?> doIgnore -- equivalent to manageDocks
    , isDialog               -?> doFloat
    , className =? "Gimp"    -?> doFloat
    , className =? "MPlayer" -?> doFloat
    , return True -?> doF W.swapDown
    ]
-- insertPosition End Newer <+> composeAll
--   [ isDialog                          --> insertPosition Master Newer <+> doCenterFloat
--   , className =?   "mpv"              --> insertPosition Master Newer <+> doFloat
--   , className =?   "gl"               --> insertPosition Master Newer <+> doFloat
--   , className =?   "Gimp"             --> insertPosition Master Newer <+> doFloat
--   , className =?   "Pavucontrol"      --> insertPosition Master Newer <+> doCenterFloat
--   , className =?   "Xfce4-appfinder"  --> insertPosition Master Newer <+> doCenterFloat
--   , className =?   "Pamac-manager"    --> insertPosition Master Newer <+> doCenterFloat
--   , className =?   "Rofi"             --> insertPosition Master Newer <+> doCenterFloat
--   , className =?   "Galculator"       --> insertPosition Master Newer <+> doCenterFloat
--   , title =?       "Downloads"        --> insertPosition Master Newer <+> doFloat
--   , title =?       "Save As..."       --> insertPosition Master Newer <+> doFloat
--   -- Games
--   , className =?   "riotclientux.exe" --> doCenterFloat
--   ]


-- Handle event hook -----------------------------------------------------------
myHandleEvenHook = mempty

-- Log hook --------------------------------------------------------------------
myLogHook = return ()

-- Startup hook ----------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "trayer --edge top --align right --SetPartialStrut true --width 10 \
               \ --tint 0x212121 --height 20 --alpha 0 &"

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
