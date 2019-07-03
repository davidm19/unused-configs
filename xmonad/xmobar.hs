{-|

   File: xmobar.hs
   Description: Short and sweet xmobar configuration (which is still in its infancy)
   Author: davidm19
   Date: June 18th, 2019

-}

Config { 

   -- Section: Appearance
     font =         "curie:size=11:bold:antialias=false"
   , bgColor =      "black"
   , fgColor =      "#646464"
   , position =     Top
   , border =       BottomB
   , borderColor =  "#646464"

   -- Section: Layout
   , sepChar =  "%"
   , alignSep = "}{"
   , template = "%StdinReader% | %multicpu% | %coretemp% | %memory% | %dynnetwork% }{ %date% || %kbd% "

   -- Section: general behavior
   , lowerOnStart =     True
   , hideOnStart =      False
   , allDesktops =      True
   , overrideRedirect = True
   , pickBroadest =     False
   , persistent =       True

   -- Scetion: Plugins
   , commands = 
        -- network activity monitor (dynamic interface resolution)
        [ Run DynNetwork     [ "--template" , "<dev>: <tx>kB/s|<rx>kB/s"
                             , "--Low"      , "1000"       -- units: B/s
                             , "--High"     , "5000"       -- units: B/s
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 10

        -- cpu activity monitor
        , Run MultiCpu       [ "--template" , "Cpu: <total0>%|<total1>%"
                             , "--Low"      , "50"         -- units: %
                             , "--High"     , "85"         -- units: %
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 10

        -- cpu core temperature monitor
        , Run CoreTemp       [ "--template" , "Temp: <core0>°C|<core1>°C"
                             , "--Low"      , "70"        -- units: °C
                             , "--High"     , "80"        -- units: °C
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 50
                          
        -- memory usage monitor
        , Run Memory         [ "--template" ,"Mem: <usedratio>%"
                             , "--Low"      , "20"        -- units: %
                             , "--High"     , "90"        -- units: %
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 10

        -- battery monitor
        , Run Battery        [ "--template" , "Batt: <acstatus>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "darkred"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkgreen"

                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o"	, "<left>% (<timeleft>)"
                                       -- AC "on" status
                                       , "-O"	, "<fc=#dAA520>Charging</fc>"
                                       -- charged status
                                       , "-i"	, "<fc=#006000>Charged</fc>"
                             ] 50

        -- time and date indicator 
        --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
        , Run Date           "<fc=#ABABAB>%F (%a) %T</fc>" "date" 10

        -- keyboard layout indicator
        , Run Kbd            [ ("us(dvorak)" , "<fc=#00008B>DV</fc>")
                             , ("us"         , "<fc=#8B0000>US</fc>")
                             ]
	, Run StdinReader
        ]
   }
