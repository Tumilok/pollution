%%%-------------------------------------------------------------------
%%% @author tumilok
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Apr 2020 20:59
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("tumilok").

-include_lib("eunit/include/eunit.hrl").

generateTestMonitor() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addStation("Kazimierza", {51.2345, 19.3445}, P1),
  P3 = pollution:addValue({50.2345, 18.3445}, {{2020, 4, 7}, {15, 30, 16}}, "PM10", 59, P2),
  P4 = pollution:addValue("Aleja Slowackiego", {{2020, 4, 7}, {15, 30, 17}}, "PM2,5", 113, P3),
  P5 = pollution:addValue("Kazimierza", {{2020, 4, 7}, {15, 30, 18}}, "PM2,5", 117, P4),
  pollution:addValue("Kazimierza", {{2020, 4, 7}, {15, 30, 19}}, "PM2,5", 30, P5).

createMonitor_test() ->
  ?assertEqual(#{}, pollution:createMonitor()).

addStation_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addStation("Kazimierza", {51.2345, 19.3445}, P1),
  P3 = pollution:addStation("Kazimierza", {52.2345, 20.3445}, P2),
  P4 = pollution:addStation("Random", {50.2345, 18.3445}, P2),
  ?assertEqual(#{{station,{50.2345,18.3445},"Aleja Slowackiego"} => #{}}, P1),
  ?assertEqual(#{{station,{50.2345,18.3445},"Aleja Slowackiego"} => #{},
    {station,{51.2345,19.3445},"Kazimierza"} => #{}}, P2),
  ?assertEqual({error,"Name of such a value is already exist"}, P3),
  ?assertEqual({error,"Coordinate of such a value is already exist"}, P4).

addValue_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, P),
  P2 = pollution:addStation("Kazimierza", {51.2345, 19.3445}, P1),
  P3 = pollution:addValue({50.2345, 18.3445}, {{2020, 4, 7}, {15, 30, 16}}, "PM10", 59, P2),
  P4 = pollution:addValue("Aleja Slowackiego", {{2020, 4, 7}, {15, 30, 17}}, "PM2,5", 113, P3),
  P5 = pollution:addValue("Kazimierza", {{2020, 4, 7}, {15, 30, 18}}, "PM2,5", 117, P4),
  P6 = pollution:addValue("Kazimierza", {{2020, 4, 7}, {15, 30, 19}}, "PM2,5", 30, P5),
  P7 = pollution:addValue("Kazimierza", {{2020, 4, 7}, {15, 30, 19}}, "PM2,5", 10, P6),
  P8 = pollution:addValue("random", {{2020, 4, 7}, {15, 30, 19}}, "PM2,5", 10, P6),
  ?assertEqual(#{{station,{50.2345,18.3445},"Aleja Slowackiego"} =>
  #{{measurement,"PM10",{{2020,4,7},{15,30,16}}} => 59},
    {station,{51.2345,19.3445},"Kazimierza"} => #{}}, P3),
  ?assertEqual(#{{station,{50.2345,18.3445},"Aleja Slowackiego"} =>
  #{{measurement,"PM10",{{2020,4,7},{15,30,16}}} => 59,
    {measurement,"PM2,5",{{2020,4,7},{15,30,17}}} => 113},
    {station,{51.2345,19.3445},"Kazimierza"} => #{}}, P4),
  ?assertEqual(#{{station,{50.2345,18.3445},"Aleja Slowackiego"} =>
  #{{measurement,"PM10",{{2020,4,7},{15,30,16}}} => 59,
    {measurement,"PM2,5",{{2020,4,7},{15,30,17}}} => 113},
    {station,{51.2345,19.3445},"Kazimierza"} =>
    #{{measurement,"PM2,5",{{2020,4,7},{15,30,18}}} => 117}}, P5),
  ?assertEqual(#{{station,{50.2345,18.3445},"Aleja Slowackiego"} =>
  #{{measurement,"PM10",{{2020,4,7},{15,30,16}}} => 59,
    {measurement,"PM2,5",{{2020,4,7},{15,30,17}}} => 113},
    {station,{51.2345,19.3445},"Kazimierza"} =>
    #{{measurement,"PM2,5",{{2020,4,7},{15,30,18}}} => 117,
      {measurement,"PM2,5",{{2020,4,7},{15,30,19}}} => 30}}, P6),
  ?assertEqual({error, "Type and Date of such value are already exist"}, P7),
  ?assertEqual({error, "Station doesn't exist"}, P8).

removeValue_test() ->
  Monitor = generateTestMonitor(),
  ?assertEqual(#{{station,{50.2345,18.3445},"Aleja Slowackiego"} =>
  #{{measurement,"PM10",{{2020,4,7},{15,30,16}}} => 59,
    {measurement,"PM2,5",{{2020,4,7},{15,30,17}}} => 113},
    {station,{51.2345,19.3445},"Kazimierza"} =>
    #{{measurement,"PM2,5",{{2020,4,7},{15,30,19}}} => 30}},
    pollution:removeValue({51.2345,19.3445}, {{2020,4,7},{15,30,18}}, "PM2,5", Monitor)),
  ?assertEqual(#{{station,{50.2345,18.3445},"Aleja Slowackiego"} =>
  #{{measurement,"PM2,5",{{2020,4,7},{15,30,17}}} => 113},
    {station,{51.2345,19.3445},"Kazimierza"} =>
    #{{measurement,"PM2,5",{{2020,4,7},{15,30,18}}} => 117,
      {measurement,"PM2,5",{{2020,4,7},{15,30,19}}} => 30}},
    pollution:removeValue("Aleja Slowackiego", {{2020,4,7},{15,30,16}}, "PM10", Monitor)),
  ?assertEqual({error, "Measurements with such parameters don't exist"}, pollution:removeValue("Aleja Slowackiego", {{2020,4,7},{15,30,16}}, "PM2,5", Monitor)),
  ?assertEqual({error, "Station doesn't exist"}, pollution:removeValue("Rand", {{2020,4,7},{15,30,16}}, "PM10", Monitor)).

getOneValue_test() ->
  Monitor = generateTestMonitor(),
  ?assertEqual(59, pollution:getOneValue("Aleja Slowackiego", {{2020, 4, 7},{15,30,16}}, "PM10", Monitor)),
  ?assertEqual(117, pollution:getOneValue({51.2345,19.3445}, {{2020, 4, 7},{15,30,18}}, "PM2,5", Monitor)),
  ?assertEqual({error, "Measurements with such parameters don't exist"}, pollution:getOneValue("Aleja Slowackiego", {{2020, 4, 7},{15,30,17}}, "PM10", Monitor)),
  ?assertEqual({error,"Station doesn't exist"}, pollution:getOneValue("Random", {{2020, 4, 7},{15,30,17}}, "PM2,5", Monitor)).

getStationMean_test() ->
  Monitor = generateTestMonitor(),
  ?assertEqual(59.0, pollution:getStationMean({50.2345,18.3445}, "PM10", Monitor)),
  ?assertEqual(73.5, pollution:getStationMean("Kazimierza", "PM2,5", Monitor)),
  ?assertEqual({error, "Station doesn't exist"}, pollution:getStationMean({50.0000,18.0000}, "PM10", Monitor)),
  ?assertEqual(0.0, pollution:getStationMean("Aleja Slowackiego", "Rand", Monitor)).

getDailyMean_test() ->
  Monitor = generateTestMonitor(),
  ?assertEqual(86.66666666666667, pollution:getDailyMean("PM2,5", {2020, 4, 7}, Monitor)),
  ?assertEqual(59.0, pollution:getDailyMean("PM10", {2020, 4, 7}, Monitor)),
  ?assertEqual(0.0, pollution:getDailyMean("PM2,5", {2020, 4, 8}, Monitor)),
  ?assertEqual(0.0, pollution:getDailyMean("Rand", {2020, 4, 7}, Monitor)).

getHourlyMean_test() ->
  Monitor = generateTestMonitor(),
  ?assertEqual(59.0, pollution:getHourlyMean("PM10", 15, Monitor)),
  ?assertEqual(86.66666666666667, pollution:getHourlyMean("PM2,5", 15, Monitor)),
  ?assertEqual(0.0, pollution:getHourlyMean("PM10", 12, Monitor)),
  ?assertEqual(0.0, pollution:getHourlyMean("Rand", 15, Monitor)).

getDailyAverageDataCount_test() ->
  Monitor = generateTestMonitor(),
  ?assertEqual(2, pollution:getDailyAverageDataCount("Aleja Slowackiego", {2020, 4, 7}, Monitor)),
  ?assertEqual(2, pollution:getDailyAverageDataCount({51.2345,19.3445}, {2020, 4, 7}, Monitor)),
  ?assertEqual(0, pollution:getDailyAverageDataCount("Aleja Slowackiego", {2020, 4, 9}, Monitor)),
  ?assertEqual({error,"Station doesn't exist"}, pollution:getDailyAverageDataCount("Rand", {2020, 4, 7}, Monitor)).

getDailyOverLimit_test() ->
  Monitor = generateTestMonitor(),
  ?assertEqual(1, pollution:getDailyOverLimit({2020,4,7}, "PM10", 50, Monitor)),
  ?assertEqual(0, pollution:getDailyOverLimit({2020,4,7}, "PM10", 70, Monitor)),
  ?assertEqual(2, pollution:getDailyOverLimit({2020,4,7}, "PM2,5", 32, Monitor)),
  ?assertEqual(0, pollution:getDailyOverLimit({2020,4,8}, "PM2,5", 5, Monitor)),
  ?assertEqual(0, pollution:getDailyOverLimit({2020,4,7}, "Rand", 5, Monitor)).

getMaximumGradientStations_test() ->
  Monitor = generateTestMonitor(),
  ?assertEqual(0, pollution:getMaximumGradientStations("PM10", Monitor)),
  ?assertEqual(87, pollution:getMaximumGradientStations("PM2,5", Monitor)),
  ?assertEqual(error, pollution:getMaximumGradientStations("Rand", Monitor)).

getMinValue_test() ->
  Monitor = generateTestMonitor(),
  ?assertEqual(30, pollution:getMinValue("PM2,5", Monitor)),
  ?assertEqual(59, pollution:getMinValue("PM10", Monitor)),
  ?assertEqual(error, pollution:getMinValue("Rand", Monitor)).

getMaxValue_test() ->
  Monitor = generateTestMonitor(),
  ?assertEqual(59, pollution:getMaxValue("PM10", Monitor)),
  ?assertEqual(117, pollution:getMaxValue("PM2,5", Monitor)),
  ?assertEqual(error, pollution:getMaxValue("Rand", Monitor)).