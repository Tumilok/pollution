%%%-------------------------------------------------------------------
%%% @author tumilok
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2020 12:34
%%%-------------------------------------------------------------------
-module(pollution_gen_server_test).
-author("tumilok").

-include_lib("eunit/include/eunit.hrl").

addStationsAndValuesToServer_test() ->
  ?assertEqual(ok, pollution_gen_server:addStation("Aleja Slowackiego", {50.2345, 18.3445})),
  ?assertEqual(ok, pollution_gen_server:addStation("Kazimierza", {51.2345, 19.3445})),
  ?assertEqual(ok, pollution_gen_server:addValue({50.2345, 18.3445}, {{2020, 4, 7}, {15, 30, 16}}, "PM10", 59)),
  ?assertEqual(ok, pollution_gen_server:addValue("Aleja Slowackiego", {{2020, 4, 7}, {15, 30, 17}}, "PM2,5", 113)),
  ?assertEqual(ok, pollution_gen_server:addValue("Kazimierza", {{2020, 4, 7}, {15, 30, 18}}, "PM2,5", 117)),
  ?assertEqual(ok, pollution_gen_server:addValue("Kazimierza", {{2020, 4, 7}, {15, 30, 19}}, "PM2,5", 30)).

removeValue_test() ->
  ?assertEqual(ok, pollution_gen_server:removeValue({51.2345,19.3445}, {{2020,4,7},{15,30,18}}, "PM2,5")),
  pollution_gen_server:addValue("Kazimierza", {{2020, 4, 7}, {15, 30, 18}}, "PM2,5", 117),
  ?assertEqual(ok, pollution_gen_server:removeValue("Aleja Slowackiego", {{2020,4,7},{15,30,16}}, "PM10")),
  pollution_gen_server:addValue({50.2345, 18.3445}, {{2020, 4, 7}, {15, 30, 16}}, "PM10", 59).

getOneValue_test() ->
  ?assertEqual(59, pollution_gen_server:getOneValue("Aleja Slowackiego", {{2020, 4, 7},{15,30,16}}, "PM10")),
  ?assertEqual(117, pollution_gen_server:getOneValue({51.2345,19.3445}, {{2020, 4, 7},{15,30,18}}, "PM2,5")).

getStationMean_test() ->
  ?assertEqual(59.0, pollution_gen_server:getStationMean({50.2345,18.3445}, "PM10")),
  ?assertEqual(73.5, pollution_gen_server:getStationMean("Kazimierza", "PM2,5")).

getDailyMean_test() ->
  ?assertEqual(86.66666666666667, pollution_gen_server:getDailyMean("PM2,5", {2020, 4, 7})),
  ?assertEqual(59.0, pollution_gen_server:getDailyMean("PM10", {2020, 4, 7})).

getHourlyMean_test() ->
  ?assertEqual(59.0, pollution_gen_server:getHourlyMean("PM10", 15)),
  ?assertEqual(86.66666666666667, pollution_gen_server:getHourlyMean("PM2,5", 15)).

getDailyAverageDataCount_test() ->
  ?assertEqual(2, pollution_gen_server:getDailyAverageDataCount("Aleja Slowackiego", {2020, 4, 7})),
  ?assertEqual(2, pollution_gen_server:getDailyAverageDataCount({51.2345,19.3445}, {2020, 4, 7})).

getDailyOverLimit_test() ->
  ?assertEqual(1, pollution_gen_server:getDailyOverLimit({2020,4,7}, "PM10", 50)),
  ?assertEqual(0, pollution_gen_server:getDailyOverLimit({2020,4,7}, "PM10", 70)).

getMaximumGradientStations_test() ->
  ?assertEqual(0, pollution_gen_server:getMaximumGradientStations("PM10")),
  ?assertEqual(87, pollution_gen_server:getMaximumGradientStations("PM2,5")).

getMinValue_test() ->
  ?assertEqual(30, pollution_gen_server:getMinValue("PM2,5")),
  ?assertEqual(59, pollution_gen_server:getMinValue("PM10")).

getMaxValue_test() ->
  ?assertEqual(59, pollution_gen_server:getMaxValue("PM10")),
  ?assertEqual(117, pollution_gen_server:getMaxValue("PM2,5")).

addExistingStation_test() ->
  pollution_gen_server:addStation("Kazimierza", {51.2346, 29.3445}),
  ?assertEqual(73.5, pollution_gen_server:getStationMean("Kazimierza", "PM2,5")).