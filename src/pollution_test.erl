%%%-------------------------------------------------------------------
%%% @author tumilok
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2020 15:56
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("tumilok").

%% API
-export([test/0]).

% function tests pollution module by firstly executing
% all function and then comparing result from execution
% with expected values
test() ->
  % executing all functions
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addStation("Kazimierza", {51.2345, 19.3445}, P1),
  P3 = pollution:addValue({50.2345, 18.3445}, {{2020, 4, 7}, {15, 30, 16}}, "PM10", 59, P2),
  P4 = pollution:addValue("Aleja Slowackiego", {{2020, 4, 7}, {15, 30, 17}}, "PM2,5", 113, P3),
  P5 = pollution:addValue("Kazimierza", {{2020, 4, 7}, {15, 30, 18}}, "PM2,5", 117, P4),
  P6 = pollution:addValue("Kazimierza", {{2020, 4, 7}, {15, 30, 19}}, "PM2,5", 30, P5),
  P7 = pollution:removeValue({51.2345,19.3445}, {{2020,4,7},{15,30,18}}, "PM2,5", P6),

  Value = pollution:getOneValue("Kazimierza", {{2020, 4, 7},{15,30,18}}, "PM2,5", P6),
  StationMean = pollution:getStationMean({50.2345,18.3445}, "PM10", P6),
  HourlyMean = pollution:getHourlyMean("PM2,5", 15, P6),
  DailyMean = pollution:getDailyMean("PM2,5", {2020, 4, 7}, P6),
  Number = pollution:getDailyAverageDataCount("Kazimierza", {2020, 4, 7}, P6),
  Diff = pollution:getMaximumGradientStations("PM2,5", P6),
  MinValue = pollution:getMinValue("PM2,5", P6),
  MaxValue = pollution:getMaxValue("PM2,5", P6),
  OverLimitStations = pollution:getDailyOverLimit({2020,4,7}, "PM2,5", 115, P6),

  % comparing results
  P = #{},
  P1 = #{{station,{50.2345,18.3445},"Aleja Slowackiego"} => #{}},
  P2 =
    #{{station,{50.2345,18.3445},"Aleja Slowackiego"} => #{},
      {station,{51.2345,19.3445},"Kazimierza"} => #{}},
  P3 =
    #{{station,{50.2345,18.3445},"Aleja Slowackiego"} =>
    #{{measurement,"PM10",{{2020,4,7},{15,30,16}}} => 59},
      {station,{51.2345,19.3445},"Kazimierza"} => #{}},
  P4 =
    #{{station,{50.2345,18.3445},"Aleja Slowackiego"} =>
    #{{measurement,"PM10",{{2020,4,7},{15,30,16}}} => 59,
      {measurement,"PM2,5",{{2020,4,7},{15,30,17}}} => 113},
      {station,{51.2345,19.3445},"Kazimierza"} => #{}},
  P5 =
    #{{station,{50.2345,18.3445},"Aleja Slowackiego"} =>
    #{{measurement,"PM10",{{2020,4,7},{15,30,16}}} => 59,
      {measurement,"PM2,5",{{2020,4,7},{15,30,17}}} => 113},
      {station,{51.2345,19.3445},"Kazimierza"} =>
      #{{measurement,"PM2,5",{{2020,4,7},{15,30,18}}} => 117}},
  P6 =
    #{{station,{50.2345,18.3445},"Aleja Slowackiego"} =>
    #{{measurement,"PM10",{{2020,4,7},{15,30,16}}} => 59,
      {measurement,"PM2,5",{{2020,4,7},{15,30,17}}} => 113},
      {station,{51.2345,19.3445},"Kazimierza"} =>
      #{{measurement,"PM2,5",{{2020,4,7},{15,30,18}}} => 117,
        {measurement,"PM2,5",{{2020,4,7},{15,30,19}}} => 30}},
  P7 =
    #{{station,{50.2345,18.3445},"Aleja Slowackiego"} =>
    #{{measurement,"PM10",{{2020,4,7},{15,30,16}}} => 59,
      {measurement,"PM2,5",{{2020,4,7},{15,30,17}}} => 113},
      {station,{51.2345,19.3445},"Kazimierza"} =>
      #{{measurement,"PM2,5",{{2020,4,7},{15,30,19}}} => 30}},

  Value = 117,
  StationMean = 59.0,
  HourlyMean = 86.66666666666667,
  DailyMean = 86.66666666666667,
  Number = 2,
  Diff = 87,
  MinValue = 30,
  MaxValue = 117,
  OverLimitStations = 1,

  % tests passed
  ok.
