import System.Exit
import Data.Ratio ((%))

import XMonad
import XMonad.Config.Azerty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.Script
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.IM

import XMonad.Hooks.EwmhDesktops (ewmh)
import System.Taffybar.Hooks.PagerHints (pagerHints)
import XMonad.Hooks.ManageDocks

import System.Taffybar.XMonadLog ( dbusLog )

import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W

import Data.Monoid
import qualified Data.Map as M


main :: IO ()
main =
    xmonad $
    ewmh $
    pagerHints $
    defaultConfig { keys = bepoKeys
                  , terminal = "urxvt"
                  , workspaces = ["1","2","3","4","5","6","chat","mail","web"]
                  , manageHook = myManageHook <+> manageDocks
                  , layoutHook = avoidStruts myLayoutHook
                  , handleEventHook = (handleEventHook defaultConfig) <+> docksEventHook
		  , startupHook = execScriptHook "startup"
                  }
 
myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
        [ className =? "qemu-system-x86_64" --> doFloat
        ]

myLayoutHook = layoutHook defaultConfig ||| Grid ||| noBorders Full ||| withIM (1%6) (Role "roster") Grid

numBepo = [0x22,0xab,0xbb,0x28,0x29,0x40,0x2b,0x2d,0x2f,0x2a]
-- numAzerty = [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0]
num = numBepo

bepoKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
bepoKeys conf@(XConfig {modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modm,               xK_p     ), spawn "dmenu_run") -- %! Launch dmenu
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

    , ((modm .|. shiftMask, xK_e     ), io exitSuccess) -- %! Quit xmonad
    , ((modm              , xK_e     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad

    , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+")
    , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
    , ((modm, xK_b     ), sendMessage ToggleStruts)
    -- , ((0, xF86XK_AudioLowerVolume), lowerVolume 5 >> return ()) -- spawn "amixer set Master 5%-"
    -- , ((0, xF86XK_AudioRaiseVolume), raiseVolume 5 >> return ()) -- spawn "amixer set Master 5%+"
    -- , ((0, xF86XK_AudioMute), toggleMute >> return ())           -- spawn "amixer toggle"
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (workspaces conf) num
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
