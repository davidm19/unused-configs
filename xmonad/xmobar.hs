Config { 

   -- appearance
     -- font =         "xft:xos4 Terminus:size=11:antialias=True"
     font        = "xft:Inconsolata:size=11:antialias=True"
   , bgColor     = "#050505"
   , fgColor     = "#646464"
   , position    = Top
   , border      = BottomB
   , borderColor = "#121212"

   -- Section: Layout
   , sepChar  =  "%"
   , alignSep = "}{"
   , template = "%StdinReader% }{ %dynnetwork% %multicpu% <fc=#b2b2b2>::</fc> %memory% <fc=#b2b2b2>::</fc> %date% "

   -- Section: general behavior
   , lowerOnStart     = True
   , hideOnStart      = False
   , allDesktops      = True
   , overrideRedirect = True
   , pickBroadest     = False
   , persistent       = True

   -- Scetion: Plugins
   , commands = 
        -- network activity monitor (dynamic interface resolution)
        [ Run DynNetwork     [ "--template" , "<fc=#949494><dev>:</fc> <tx><fc=#949494>kB/s|</fc><rx><fc=#949494>kB/s ::</fc>"
                             , "--Low"      , "1000"       -- units: B/s
                             , "--High"     , "5000"       -- units: B/s
                             , "--low"      , "green"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 10

        -- cpu activity monitor
        , Run MultiCpu       [ "--template" , "<fc=#949494>CPU:</fc> <total0><fc=#949494>%|</fc><total1><fc=#949494>%</fc>"
                             , "--Low"      , "50"         -- units: %
                             , "--High"     , "85"         -- units: %
                             , "--low"      , "green"
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
        , Run Memory         [ "--template" ,"<fc=#949494>Mem:</fc> <usedratio><fc=#949494>%</fc>"
                             , "--Low"      , "20"        -- units: %
                             , "--High"     , "90"        -- units: %
                             , "--low"      , "green"
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
        , Run Date           "<fc=#00ffff>%Y-%h-%d</fc> <fc=#ffff00>(%a)</fc> <fc=#ff0000>%T</fc>" "date" 10

        -- keyboard layout indicator
        , Run Kbd            [ ("us(dvorak)" , "<fc=#00008B>DV</fc>")
                             , ("us"         , "<fc=#8B0000>US</fc>")
                             ]
	, Run StdinReader
        ]
   }
