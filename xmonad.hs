{-# LANGUAGE OverloadedStrings #-}

import System.Exit
import Data.Ratio ((%))
import Data.Int

import XMonad
import XMonad.Config.Azerty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.Script
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.IM
import XMonad.Hooks.ManageHelpers
import DBus
import DBus.Client

import XMonad.Hooks.EwmhDesktops (ewmh)
-- import System.Taffybar.Hooks.PagerHints (pagerHints)
import XMonad.Hooks.ManageDocks

-- import System.Taffybar.XMonadLog ( dbusLog )

import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W

import Data.Monoid
import qualified Data.Map as M


lockScreen :: IO ()
lockScreen = do
  client <- connectSession
  callNoReply client (methodCall "/ScreenSaver" "org.freedesktop.ScreenSaver" "Lock")
   {  methodCallDestination = Just "org.freedesktop.ScreenSaver"
   }
  return ()

logOut :: IO ()
logOut = do
  client <- connectSession
  callNoReply client (methodCall "/KSMServer" "org.kde.KSMServerInterface" "logout")
   { methodCallDestination = Just "org.kde.ksmserver"
   , methodCallBody = [ toVariant (1 :: Int32)
                      , toVariant (0 :: Int32)
                      , toVariant (30 :: Int32)
                      ]
   }

main :: IO ()
main =
    xmonad $
    ewmh $
    -- pagerHints $
    defaultConfig { keys = bepoKeys
                  , terminal = "konsole"
                  , workspaces = ["1","2","3","4","5","6","chat","mail","web"]
                  , manageHook = myManageHook <+> manageDocks
                  , layoutHook = avoidStruts myLayoutHook
                  , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
		  , startupHook = execScriptHook "startup"
                  }
 
myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
        [ className =? "qemu-system-x86_64" --> doFloat
        , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NOTIFICATION" --> doFloat
        ]

myLayoutHook = layoutHook defaultConfig ||| Grid ||| noBorders Full ||| withIM (1%6) (Role "roster") Grid

numBepo = [0x22,0xab,0xbb,0x28,0x29,0x40,0x2b,0x2d,0x2f,0x2a]
-- numAzerty = [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0]
num = numBepo

bepoKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
bepoKeys conf@XConfig {modMask = modm} = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    -- , ((modm,               xK_p     ), spawn "dmenu_run") -- %! Launch dmenu
    , ((modm, xK_p), spawn "krunner")
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun") -- %! Launch gmrun
    , ((modm .|. shiftMask, xK_c     ), kill) -- %! Close the focused window

    , ((modm,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ((modm,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

    , ((modm,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modm, xK_c), sendMessage Shrink          )
    , ((modm, xK_n), windows     W.focusMaster   )
    , ((modm, xK_t), windows     W.focusDown     )
    , ((modm, xK_s), windows     W.focusUp       )
    , ((modm, xK_r), sendMessage Expand          )

    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_t),      windows W.swapDown  )
    , ((modm .|. shiftMask, xK_s),      windows W.swapUp    )

    , ((modm, xK_g), sendMessage (IncMasterN (-1)))
    , ((modm, xK_q), sendMessage (IncMasterN 1)  )

    , ((modm, xK_f), withFocused $ windows . W.sink) -- %! Push window back into tiling

    --, ((modm .|. shiftMask, xK_e     ), io exitSuccess) -- %! Quit xmonad
    , ((modm .|. shiftMask, xK_e     ), io logOut) -- %! Quit xmonad
    , ((modm .|. shiftMask, xK_q), spawn "dcop kdesktop default logout")
    , ((modm .|. shiftMask, xK_l), io lockScreen)
    , ((modm              , xK_e     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad

    , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+")
    , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
    , ((modm, xK_b     ), sendMessage ToggleStruts)
    , ((modm .|. shiftMask, xK_agrave), spawn "i3lock -c 000000")
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (workspaces conf) num
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
       | (key, sc) <- zip [xK_egrave,xK_o] [0..]
       , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
