-- minimal Ubuntu config file: ~/.xmonad/xmonad.hs
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)  
import XMonad.Util.EZConfig (additionalKeys)
import System.IO
import System.Exit
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ScreenCorners
-- import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Decoration
import XMonad.Layout.Tabbed
import XMonad.Util.Run(spawnPipe,safeSpawn)
import XMonad.Util.Themes
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Config.Desktop
import System.Environment (getEnvironment)
import XMonad.Prompt
import XMonad.Prompt.Window
import qualified XMonad.Actions.GridSelect as G
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import XMonad.Layout.Spacing
import XMonad.Layout.IndependentScreens
-- import XMonad.Hooks.ICCCMFocus
import qualified XMonad.Util.Dzen as Dzen

import XMonad.Util.Run (runProcessWithInput)

main = do
   xmproc <- spawnPipe "xmobar /home/gyorgy/.xmobarrc" 
   xmonad $ docks def
       { manageHook = manageDocks <+> manageHook def
       , layoutHook = avoidStruts $ layoutHook def
       , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                         , ppTitle = xmobarColor "green" "" . shorten 50
                          }
       , modMask     = mod4Mask -- set 'Mod' to windows key
       , terminal    = "gnome-terminal" -- for Mod + Shift + Enter
       }
