%%%-------------------------------------------------------------------
%%% @author tumilok
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2020 10:02
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-behavior(gen_server).
-author("tumilok").

-export([
  start_link/0,
  addStation/2,
  addValue/4,
  removeValue/3,
  getOneValue/3,
  getStationMean/2,
  getDailyMean/2,
  getHourlyMean/2,
  getDailyAverageDataCount/2,
  getDailyOverLimit/3,
  getMaximumGradientStations/1,
  getMinValue/1,
  getMaxValue/1
]).
-export([init/1, handle_call/3, handle_cast/2]).


start_link() -> gen_server:start_link(?MODULE, [], []).

%% Asynchronous call
addStation(Name, Coords) ->
  gen_server:call(addStation, {Name, Coords}).

%% Asynchronous call
addValue(Key, Date, Type, Val) ->
  gen_server:call(addValue, {Key, Date, Type, Val}).

%% Asynchronous call
removeValue(Key, Date, Type) ->
  gen_server:call(removeValue, {Key, Date, Type}).

%% Synchronous call
getOneValue(Key, Date, Type) ->
  gen_server:call(getOneValue, {Key, Date, Type}).

%% Synchronous call
getStationMean(Key, Type) ->
  gen_server:call(getStationMean, {Key, Type}).

%% Synchronous call
getDailyMean(Type, Day) ->
  gen_server:call(getDailyMean, {Type, Day}).

%% Synchronous call
getHourlyMean(Type, Hour) ->
  gen_server:call(getHourlyMean, {Type, Hour}).

%% Synchronous call
getDailyAverageDataCount(Key, Date) ->
  gen_server:call(getDailyAverageDataCount, {Key, Date}).

%% Synchronous call
getDailyOverLimit(Date, Type, Limit) ->
  gen_server:call(getDailyOverLimit, {Date, Type, Limit}).

%% Synchronous call
getMaximumGradientStations(Type) ->
  gen_server:call(getMaximumGradientStations, {Type}).

%% Synchronous call
getMinValue(Type) ->
  gen_server:call(getMinValue, {Type}).

%% Synchronous call
getMaxValue(Type) ->
  gen_server:call(getMaxValue, {Type}).

%%% Server functions
init([]) -> {ok, pollution:createMonitor()}.

handle_call({getOneValue, Key, Date, Type}, _From, Monitor) ->
  {reply, pollution:getOneValue(Key, Date, Type, Monitor), Monitor};
handle_call({getStationMean, {Key, Type}}, _From, Monitor) ->
  {reply, pollution:getStationMean(Key, Type, Monitor), Monitor};
handle_call({getDailyMean, Type, Day}, _From, Monitor) ->
  {reply, pollution:getDailyMean(Type, Day, Monitor), Monitor};
handle_call({getHourlyMean, Type, Hour}, _From, Monitor) ->
  {reply, pollution:getHourlyMean(Type, Hour, Monitor), Monitor};
handle_call({getDailyAverageDataCount, Key, Date}, _From, Monitor) ->
  pollution:getDailyAverageDataCount(Key, Date, Monitor);
handle_call({getDailyOverLimit, Date, Type, Limit}, _From, Monitor) ->
  pollution:getDailyOverLimit(Date, Type, Limit, Monitor);
handle_call({getMaximumGradientStations, Type}, _From, Monitor) ->
  pollution:getMaximumGradientStations(Type, Monitor);
handle_call({getMinValue, Type}, _From, Monitor) ->
  pollution:getMinValue(Type, Monitor);
handle_call({getMaxValue, Type}, _From, Monitor) ->
  pollution:getMaxValue(Type, Monitor).


handle_cast({addStation, Name, Coords}, Monitor) ->
  case pollution:addStation(Name, Coords, Monitor) of
    {error, Msg} ->
      io:format("Error: ~p~n", [Msg]),
      {noreply, Monitor};
    NewMonitor ->
      {noreply, NewMonitor}
  end;
handle_cast({addValue, Key, Date, Type, Val}, Monitor) ->
  case pollution:addValue(Key, Date, Type, Val, Monitor) of
    {error, Msg} ->
      io:format("Error: ~p~n", [Msg]),
      {noreply, Monitor};
    NewMonitor ->
      {noreply, NewMonitor}
  end;
handle_cast({removeValue, Key, Date, Type}, Monitor) ->
  case pollution:removeValue(Key, Date, Type, Monitor) of
    {error, Msg} ->
      io:format("Error: ~p~n", [Msg]),
      {noreply, Monitor};
    NewMonitor ->
      {noreply, NewMonitor}
  end;
handle_cast(terminate, Monitor) ->
  {stop, normal, Monitor};
handle_cast(crash, _Monitor) ->
  {noreply, 1/0}.