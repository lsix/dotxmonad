import XMonad
import XMonad.Config.Azerty
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Grid

import Data.Monoid
import qualified Data.Map as M

main :: IO ()
main = statusBar myBar myPP toggleStrutsKey myConfig  >>=
       xmonad

myConfig = defaultConfig { keys = \c -> azertyKeys c `M.union` keys defaultConfig c
			 , terminal = "konsole"
			 , workspaces = ["1","2","3","4","5","6","chat","mail","web"]
			 , manageHook = manageHook defaultConfig <+> myManageHook
			 , layoutHook = myLayoutHook
			 }

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
	[ className =? "qemu-system-x86_64" --> doFloat
 	]

myPP = xmobarPP { ppOutput = putStrLn
		, ppCurrent = xmobarColor "orange" "" . wrap "[" "]"
		, ppHiddenNoWindows = xmobarColor "grey" ""
		, ppTitle   = xmobarColor "green"  "" . shorten 40
		, ppVisible = wrap "(" ")"
		, ppUrgent  = xmobarColor "red" "yellow" . wrap "<" ">"
		}
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myBar = "/home/lsix/.nix-profile/bin/xmobar"

myLayoutHook = layoutHook defaultConfig ||| Grid
