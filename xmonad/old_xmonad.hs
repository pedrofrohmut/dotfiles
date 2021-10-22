import XMonad
import Data.Monoid
import System.Exit

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Bar imports
import XMonad.Hooks.DynamicLog

-- Main Function --------------------------------------------------------------------------------
main :: IO ()
main = xmonad 
     . ewmh 
     =<< statusBar "xmobar" def toggleStrutsKey myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)

-- LayoutHook -----------------------------------------------------------------------------------
myLayouts = tiled ||| Mirror tiled ||| Full
  where
    nmaster = 1      -- Default number of windows in the master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    tiled   = Tall nmaster delta ratio

-- Manage hook ----------------------------------------------------------------------------------
-- use $ xprop | grep WM_CLASS then click to get the name to type
myManageHook = composeAll
  [ className =? "mpv" --> doFloat
  , isDialog           --> doFloat
  ]

-- Handle event hook ----------------------------------------------------------------------------
myHandleEvenHook = fullscreenEventHook

-- Log hook -------------------------------------------------------------------------------------
myLogHook = return ()

-- Startup hook ---------------------------------------------------------------------------------
myStartupHook = do
  spawnOnce "trayer --edge top --align right --height 18 --SetPartialStrut true --width 5 --padding 4 --tint #212121"

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

    -- Apps
    , ((mod4Mask, xK_Return), spawn "alacritty")
    , ((mod4Mask, xK_p),      spawn "rofi -modi drun -show drun -theme ~/.config/rofi/themes/my_dracula.rasi")

    -- Audio/Volume
    , ((mod4Mask, xK_minus), spawn "pamixer --decrease 5")
    , ((mod4Mask, xK_equal), spawn "pamixer --increase 5")
    , ((mod4Mask, xK_0),     spawn "pamixer --toggle-mute")
    , ((mod4Mask, xK_F9),    spawn "pacmd set-default-sink 0")
    , ((mod4Mask, xK_F10),   spawn "pacmd set-default-sink 1")

    -- Session managment
    , ((mod4Mask .|. shiftMask, xK_z),  spawn "i3lock -i /home/pedro/media/images/wallpaper/lock.png -u")
    , ((mod4Mask .|. shiftMask, xK_r),  spawn "killall xmobar; killall trayer; xmonad --recompile; xmonad --restart")
    , ((mod4Mask .|. shiftMask, xK_F3), spawn "systemctl suspend")
    , ((mod4Mask .|. shiftMask, xK_F4), io (exitWith ExitSuccess))
    ]
