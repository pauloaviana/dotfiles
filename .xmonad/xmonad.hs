---IMPORTS

    -- Base
import XMonad hiding ( (|||) ) -- 'hiding' because there is a conflict with LayoutCombinators
import XMonad.Config.Desktop
import Data.Monoid
import Data.Maybe (isJust)
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import Control.Monad (liftM2)
import qualified XMonad.StackSet as W
import qualified Data.Map as M

    -- Utils
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (runInTerm, spawnPipe)
import XMonad.Util.Loggers
import XMonad.Util.EZConfig (additionalKeysP) --For parsing keybindigs in a different format  
import XMonad.Util.NamedScratchpad

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, defaultPP, wrap, pad, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat) 
import XMonad.Hooks.EwmhDesktops --Useful with some applications

    -- Actions
import XMonad.Actions.Promote --Promotes the focused window to the master pane
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies, runOrCopy) -- Bindings to duplicate a window on multiple workspaces
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen)
import XMonad.Actions.Submap --Create sub-maps of key bindings
import XMonad.Actions.MouseResize --Resizes windows with the mouse
import qualified XMonad.Actions.Search as S

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid)) --Grid Layout
import XMonad.Layout.SimplestFloat --Float Layout
import XMonad.Layout.ThreeColumns --ThreeColums Layout
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.Spacing (spacing) 
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit) --Set limits to the number of windows that can be shown
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..)) --Resize windows in any Layout
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.LayoutCombinators --Comine Layouts and Jump directly to a Layout

    -- Prompts
import XMonad.Prompt 
import XMonad.Prompt.Shell

    -- Pywal Colors
import Colors

------------------------------------------------------------------------
---CONFIG

myFont          = "xft:noto sans:pixelsize=8"
myModMask       = mod4Mask  
myTerminal      = "st"     
myTextEditor    = "vim"    
myBrowser       = "qutebrowser"
myBorderWidth   = 2        
windowCount     = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

main = do
    xmproc <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"
    xmonad $ ewmh desktopConfig
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayout
        , workspaces         = myWorkspaces
        , handleEventHook    = handleEventHook desktopConfig <+> fullscreenEventHook
        , borderWidth        = myBorderWidth
        , normalBorderColor  = color0
        , focusedBorderColor = color2
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = \x -> hPutStrLn xmproc x               -- >> hPutStrLn xmproc x
                        , ppCurrent = xmobarColor color1  "" . wrap " \xf7a5 " "\xf7a5 " -- Current workspace in xmobar - Wrap under Font Awesome's grip-lines-vertical
                        , ppVisible = xmobarColor color2 ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor color2 "" . wrap "" ""   -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor color7 ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#d0d0d0" "" . shorten 80     -- Title of active window in xmobar
                        , ppSep =  "<fc=#A58F66> | </fc>"                     -- Separators in xmobar
                        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        , ppSort = fmap (.namedScratchpadFilterOutWorkspace) $ ppSort xmobarPP -- Hides the 'NSP' workspace created by opening a Scratchpad   
                        }
        } `additionalKeysP`         myKeys

------------------------------------------------------------------------
---STARTUP

myStartupHook = do
          spawnOnce "unclutter --timeout 3 --ignore-scrolling --jitter 150 &"
          spawnOnce "redshift -P -O 4500k &"
          spawnOnce "transmission-daemon &"
          spawnOnce "$HOME/.local/bin/keybind"

------------------------------------------------------------------------
---KEYBINDINGS

myKeys =
    -- XMonad
        [ ("M-C-r", spawn "xmonad --recompile")      -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")        -- Restarts xmonad
        , ("M-S-q q", io exitSuccess)                -- Quits xmonad
        , ("M-S-q s", spawn "poweroff")              -- Shutdown
        , ("M-S-q r", spawn "reboot")                -- Reboot
    
    -- Windows
        , ("M-S-c", kill1)                           -- Kill the currently focused client
        , ("M-S-a", killAll)                         -- Kill all the windows on current workspace

    -- Floating
        , ("M-<Delete>", withFocused $ windows . W.sink)  -- Push floating window back to tile.
        , ("M-S-<Delete>", sinkAll)                  -- Push ALL floating windows back to tile.

    -- Windows navigation
        , ("M-m", windows W.focusMaster)             -- Move focus to the master window
        , ("M-j", windows W.focusDown)               -- Move focus to the next window
        , ("M-k", windows W.focusUp)                 -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster)            -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)              -- Swap the focused window with the next window
        , ("M-S-k", windows W.swapUp)                -- Swap the focused window with the prev window
        , ("M-<Tab>", promote)                       -- Promote focused window to master, changing order
        , ("M-<Backspace>", rotSlavesDown)           -- Rotate all windows except master and keep focus in place
        , ("M-S-<Backspace>", rotAllDown)                 -- Rotate all the windows in the current stack
        , ("M-M1-x", killAllOtherCopies)             -- Kill all 'other' copies of the focused window 

    -- Layouts
        , ("M-C-h", sendMessage Shrink)                                    --Shrink Focused Window
        , ("M-C-l", sendMessage Expand)                                    --Expand Focused Window
        , ("M-d", sendMessage $ JumpToLayout "tall")                       -- Jump to the 'tall' layout aka the 'default' layout
        , ("M-f", sendMessage $ JumpToLayout "monocle")                    -- Jump to the 'monocle' layout
        , ("M-g", sendMessage $ JumpToLayout "grid")                       -- Jump to the 'grid' layout
        , ("M-s", sendMessage $ JumpToLayout "threeCol")                   -- Jump to the 'threeCol' layout
        , ("M-a", sendMessage $ Toggle MIRROR)                               -- Toggles Mirror Layouts
        , ("M-S-=", sendMessage ToggleStruts)                              -- Toggles struts
        , ("M-S-b", sendMessage $ Toggle NOBORDERS)                        -- Toggles noborder
        , ("M-<Escape>", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-<KP_Multiply>", sendMessage (IncMasterN 1))   -- Increase number of clients in the master pane
        , ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))  -- Decrease number of clients in the master pane
        , ("M-S-<KP_Multiply>", increaseLimit)              -- Increase number of windows that can be shown
        , ("M-S-<KP_Divide>", decreaseLimit)                -- Decrease number of windows that can be shown

    -- Workspaces
        , ("M-<XF86Forward>", moveTo Next nonNSP)                           -- Move to the next workspace
        , ("M-<XF86Back>", moveTo Prev nonNSP)                              -- Move to the previous workspace
        , ("M-S-<XF86Forward>", shiftTo Next nonNSP >> moveTo Next nonNSP)  -- Shifts focused window to next workspace
        , ("M-S-<XF86Back>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)     -- Shifts focused window to previous workspace
        , ("M-h", moveTo Prev nonNSP)                                       -- Move to the previous workspace
        , ("M-l", moveTo Next nonNSP)                                       -- Move to the next workspace
        , ("M-S-l", shiftTo Next nonNSP >> moveTo Next nonNSP)              -- Shifts focused window to next workspace
        , ("M-S-h", shiftTo Prev nonNSP >> moveTo Prev nonNSP)              -- Shifts focused window to previous workspace

    -- Scratchpads
        , ("M-M1-<Return>", namedScratchpadAction myScratchPads "terminal")
        , ("M-M1-d", namedScratchpadAction myScratchPads "taskell")
        , ("M-M1-p", namedScratchpadAction myScratchPads "cmus")
        , ("M-C-<Return>", namedScratchpadAction myScratchPads "vifm")
        , ("M-M1-c", namedScratchpadAction myScratchPads "calcurse")
        
    -- Terminal
        , ("M-<Return>", spawn myTerminal)
        , ("M-S-<Return>", shellPrompt myXPConfig) --Shell Prompt
    -- Dmenu 
    --    , ("M-S-<Return>", spawn ("dmenu_run" ++ " -nb '" ++ color0 ++ "' -nf '" ++ color2 ++ "' -sb '" ++ color2 ++ "' -sf '" ++ color7 ++ "'")) --dmenu_run
        , ("M-C-i", spawn ("networkmanager_dmenu" ++ " -nb '" ++ color0 ++ "' -nf '" ++ color2 ++ "' -sb '" ++ color2 ++ "' -sf '" ++ color7 ++ "'")) --NetworkManager

    ---Applications (Super+Alt+Key)
        , ("M-<Print>", spawn ("scrot -zq 100 -e 'mv $f ~/images/shots'"))
        , ("M-C-c", spawn (myTerminal ++ " -e calcurse"))
        , ("M-C-b", spawn ("$HOME/.local/bin/bmark"))
        , ("M-C-d", spawn (myTerminal ++ " -e taskell .taskell.md"))
        , ("M-C-w", spawn ("qutebrowser"))
        , ("M-C-e", spawn ("emacs"))
        , ("M-C-f", spawn (myTerminal ++ " -e vifm"))
        , ("M-C-t", spawn (myTerminal ++ " -e htop"))
        , ("M-C-n", spawn (myTerminal ++ " -e newsboat"))
        , ("M-C-p", spawn (myTerminal ++ " -e cmus"))
        , ("M-C-m", spawn (myTerminal ++ " -e neomutt"))
        , ("M-C-q", spawn ("transmission-gtk"))
        , ("M-C-s", spawn (myTerminal ++ " $HOME/.local/bin/fzfpdf.sh"))
        , ("M-C-g", spawn (myTerminal ++ " -e ghci"))
        , ("M-C-y", spawn ("killall -q xcompmgr"))
        , ("M-C-Y", spawn ("xcompmgr"))

    -- Keyboard Layouts
        , ("M-M1-1", spawn ("setxkbmap us -variant altgr-intl && $HOME/.local/bin/keybind")) -- Set default keymap
        , ("M-M1-2", spawn ("setxkbmap ru")) -- Russian
        , ("M-M1-0", spawn ("setxkbmap us -variant altgr-intl")) --US International

    -- Multimedia Keys
        , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%") --Pulseaudio Controls
        , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
        , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
        , ("<XF86AudioMicMute>", spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle")

        , ("<XF86AudioPlay>", spawn "cmus-remote -u") --Music Player Controls
        , ("<XF86AudioStop>", spawn "cmus-remote -s")
        , ("<XF86AudioPrev>", spawn "cmus-remote -r")
        , ("<XF86AudioNext>", spawn "cmus-remote -n")

        , ("<XF86ScreenSaver>", spawn ("~/.local/bin/lockscreen")) --LockScreen

        ] 
        ++ [("M-p " ++ k, S.promptSearchBrowser myXPConfig myBrowser f) | (k,f) <- searchList ] --Search Prompt Keybindings
        ++ [("M-S-p " ++ k, S.selectSearchBrowser myBrowser f) | (k,f) <- searchList ]       
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))
                
------------------------------------------------------------------------
---WORKSPACES

xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

mySpaces = [
         "1: \xf07b ", --System
         "2: \xf0ac ", --Web
         "3: \xf02d ", --Read
         "4: \xf0f4 ", --Extra
         "5: \xf1fa ", --Chat
         "6: \xf11b ", --Game
         "7: \xf144 ", --Video
         "8: \xf001 ", --Music
         "9: \xf019 "  --Sync
           ] -- Using Font Awesome

myWorkspaces :: [String]   
myWorkspaces = (map xmobarEscape) $ mySpaces

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll

-- To find the property name associated with a program, use
-- -- > xprop | grep WM_CLASS
-- -- and click on the client you're interested in.

-- To work on CLI programs, you need to use the window title in 'title' intead of
-- -- 'className' as used below.

     [
        className =? "firefox"           --> doShift (mySpaces !! 1)
      , className =? "Tor Browser"       --> doShift (mySpaces !! 1)
      , className =? "qutebrowser"       --> viewShift (mySpaces !! 1)
      , title =? "newsboat"              --> doShift (mySpaces !! 2)
      , className =? "Emacs"             --> viewShift (mySpaces !! 3)
      , title =? "neomutt"               --> doShift (mySpaces !! 4)
      , className =? "Zulip"             --> doShift (mySpaces !! 4)
      , className =? "discord"           --> doShift (mySpaces !! 4)
      , className =? "Steam"             --> doShift (mySpaces !! 5)
      , className =? "mpv"               --> viewShift (mySpaces !! 6)
      , className =? "qBittorrent"       --> doShift (mySpaces !! 8)
     ] <+> namedScratchpadManageHook myScratchPads
        where viewShift = doF . liftM2 (.) W.greedyView W.shift

------------------------------------------------------------------------
---LAYOUTS

myLayout = onWorkspace (mySpaces !! 6) monocle $
         avoidStruts $ smartBorders $ mouseResize $ windowArrange $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ 
           onWorkspaces [(mySpaces !! 1), (mySpaces !! 3)] monocle $        
           myDefaultLayout
             where 
                 myDefaultLayout = mkToggle (single MIRROR) (tall ||| monocle ||| threeCol ||| grid)

tall       = renamed [Replace "tall"]     $ limitWindows 12 $ spacing 6 $ ResizableTall 1 (3/100) (1/2) []
monocle    = renamed [Replace "monocle"]  $ limitWindows 20 $ noBorders Full
grid       = renamed [Replace "grid"]     $ limitWindows 12 $ spacing 6 $ Grid (16/10)
threeCol   = renamed [Replace "threeCol"] $ limitWindows 3  $ spacing 3 $ ThreeColMid 1 (3/100) (1/2) 
--floats     = renamed [Replace "floats"]   $ limitWindows 20 $ simplestFloat

------------------------------------------------------------------------
---SCRATCHPADS

myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm,
                  NS "taskell" spawnTaskell findTaskell manageTaskell,
                  NS "cmus" spawnCmus findCmus manageCmus,
                  NS "vifm" spawnVifm findVifm manageVifm,
                  NS "calcurse" spawnCalcurse findCalcurse manageCalcurse
                ]

    where
    spawnTerm  = myTerminal ++  " -n scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
                 where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnTaskell  = myTerminal ++  " -n scratchpad taskell .taskell.md "
    findTaskell   = resource =? "taskell"
    manageTaskell = customFloating $ W.RationalRect l t w h
                 where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCmus  = myTerminal ++  " -n cmus 'cmus' "
    findCmus   = resource =? "cmus"
    manageCmus = customFloating $ W.RationalRect l t w h
                 where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnVifm  = myTerminal ++  " -n vifm 'vifm' "
    findVifm   = resource =? "vifm"
    manageVifm = customFloating $ W.RationalRect l t w h
                 where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCalcurse  = myTerminal ++  " -n scratchpad calcurse "
    findCalcurse   = resource =? "calcurse"
    manageCalcurse = customFloating $ W.RationalRect l t w h
                 where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w

------------------------------------------------------------------------
---PROMPT

myXPConfig :: XPConfig
myXPConfig = def
      { font                = "xft:noto sans:style=Bold:size=9"
      , bgColor             = color0
      , fgColor             = color2
      , bgHLight            = color2
      , fgHLight            = color7
      , borderColor         = color1
      , promptBorderWidth   = 0
      , position            = Top
      , height              = 20
      , historySize         = 16
      , historyFilter       = deleteAllDuplicates
      , defaultText         = []
      , autoComplete        = Nothing
      , showCompletionOnTab = False
      , alwaysHighlight     = False
      , maxComplRows        = Just 1
      }

--SEARCH PROMPTS
searx, archwiki, aur, ebay, libgen, nlab, rutracker, wiktionary, hackage :: S.SearchEngine
archwiki = S.searchEngine "ArchWiki" "https://wiki.archlinux.org/index.php?search="
aur      = S.searchEngine "AUR" "https://aur.archlinux.org/packages/?K="
ebay     = S.searchEngine "ebay" "https://www.ebay.com/sch/i.html?_nkw="
libgen   = S.searchEngine "LibGen" "http://gen.lib.rus.ec/search.php?req="
nlab     = S.searchEngine "NLab" "https://ncatlab.org/nlab/search?query="
rutracker = S.searchEngine "RuTracker" "https://rutracker.org/forum/tracker.php?nm="
wiktionary = S.searchEngine "Wiktionary" "https://en.wiktionary.org/w/index.php?search="
hackage = S.searchEngine "Hackage" "http://hackage.haskell.org/packages/search?terms="
searx = S.searchEngine "SearX" "https://searx.tuxcloud.net/?q="

searchList :: [(String, S.SearchEngine)]
searchList = [ ("a", archwiki)
             , ("u", aur)
             , ("e", ebay)
             , ("l", libgen)
             , ("n", nlab)
             , ("r", rutracker)
             , ("d", wiktionary)
             , ("s", searx)
             , ("h", hackage)
             , ("w", S.wikipedia)
             , ("y", S.youtube)
             ]

