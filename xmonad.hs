import XMonad
import XMonad.Config.Azerty
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Grid
import XMonad.Layout.IM
import Data.Ratio ((%))

import Data.Monoid
import qualified Data.Map as M

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
	[ className =? "qemu-system-x86_64" --> doFloat
 	]

main :: IO ()
main = xmobar defaultConfig { keys = \c -> azertyKeys c `M.union` keys defaultConfig c
			    , terminal = "gnome-terminal"
			    , workspaces = ["1","2","3","4","5","6","chat","mail","web"]
			    , layoutHook = layoutHook defaultConfig ||| Grid ||| withIM (1%7) (ClassName "Gajim") Grid 
			    , manageHook = manageHook defaultConfig <+> myManageHook
			    , logHook = dynamicLogWithPP xmobarPP
			                  { ppOutput = putStrLn
                                          , ppCurrent = xmobarColor "orange" "" . wrap "[" "]"
                                          , ppHiddenNoWindows = xmobarColor "grey" ""
                                          , ppTitle   = xmobarColor "green"  "" . shorten 40
                                          , ppVisible = wrap "(" ")"
                                          , ppUrgent  = xmobarColor "red" "yellow" . wrap "<" ">"
                                          }
			    } >>=
       xmonad
