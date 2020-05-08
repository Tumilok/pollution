%%%-------------------------------------------------------------------
%%% @author tumilok
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2020 10:02
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("tumilok").

-behavior(gen_server).

-export([start_link/0]).

-export([
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
  getMaxValue/1,
  stop/0,
  crash/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, pollution_gen_server}, ?MODULE, [], []).

%%%-------------------------------------------------------------------
%%% Client API
%%%-------------------------------------------------------------------

%% Asynchronous call
addStation(Name, Coords) ->
  gen_server:cast(pollution_gen_server, {addStation, Name, Coords}).

%% Asynchronous call
addValue(Key, Date, Type, Val) ->
  gen_server:cast(pollution_gen_server, {addValue, Key, Date, Type, Val}).

%% Asynchronous call
removeValue(Key, Date, Type) ->
  gen_server:cast(pollution_gen_server, {removeValue, Key, Date, Type}).

%% Synchronous call
getOneValue(Key, Date, Type) ->
  gen_server:call(pollution_gen_server, {getOneValue, Key, Date, Type}).

%% Synchronous call
getStationMean(Key, Type) ->
  gen_server:call(pollution_gen_server, {getStationMean, Key, Type}).

%% Synchronous call
getDailyMean(Type, Day) ->
  gen_server:call(pollution_gen_server, {getDailyMean, Type, Day}).

%% Synchronous call
getHourlyMean(Type, Hour) ->
  gen_server:call(pollution_gen_server, {getHourlyMean, Type, Hour}).

%% Synchronous call
getDailyAverageDataCount(Key, Date) ->
  gen_server:call(pollution_gen_server, {getDailyAverageDataCount, Key, Date}).

%% Synchronous call
getDailyOverLimit(Date, Type, Limit) ->
  gen_server:call(pollution_gen_server, {getDailyOverLimit, Date, Type, Limit}).

%% Synchronous call
getMaximumGradientStations(Type) ->
  gen_server:call(pollution_gen_server, {getMaximumGradientStations, Type}).

%% Synchronous call
getMinValue(Type) ->
  gen_server:call(pollution_gen_server, {getMinValue, Type}).

%% Synchronous call
getMaxValue(Type) ->
  gen_server:call(pollution_gen_server, {getMaxValue, Type}).

%% Asynchronous call
stop() ->
  gen_server:cast(pollution_gen_server, terminate).

%% Asynchronous call
crash() ->
  gen_server:cast(pollution_gen_server, crash).

%%%-------------------------------------------------------------------
%%% Server functions
%%%-------------------------------------------------------------------

% Initialize initial data for the server
init([]) -> {ok, pollution:createMonitor()}.

% Handle synchronous calls
handle_call({getOneValue, Key, Date, Type}, _From, Monitor) ->
  {reply, pollution:getOneValue(Key, Date, Type, Monitor), Monitor};
handle_call({getStationMean, Key, Type}, _From, Monitor) ->
  {reply, pollution:getStationMean(Key, Type, Monitor), Monitor};
handle_call({getDailyMean, Type, Day}, _From, Monitor) ->
  {reply, pollution:getDailyMean(Type, Day, Monitor), Monitor};
handle_call({getHourlyMean, Type, Hour}, _From, Monitor) ->
  {reply, pollution:getHourlyMean(Type, Hour, Monitor), Monitor};
handle_call({getDailyAverageDataCount, Key, Date}, _From, Monitor) ->
  {reply, pollution:getDailyAverageDataCount(Key, Date, Monitor), Monitor};
handle_call({getDailyOverLimit, Date, Type, Limit}, _From, Monitor) ->
  {reply, pollution:getDailyOverLimit(Date, Type, Limit, Monitor), Monitor};
handle_call({getMaximumGradientStations, Type}, _From, Monitor) ->
  {reply, pollution:getMaximumGradientStations(Type, Monitor), Monitor};
handle_call({getMinValue, Type}, _From, Monitor) ->
  {reply, pollution:getMinValue(Type, Monitor), Monitor};
handle_call({getMaxValue, Type}, _From, Monitor) ->
  {reply, pollution:getMaxValue(Type, Monitor), Monitor}.

% Handle asynchronous calls
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

% Handle unknown calls
handle_info(_Info, Monitor) ->
  {noreply, Monitor}.

% Terminate server
terminate(Reason, Monitor) ->
  io:format("Server terminated with monitor: ~n~p~n", [Monitor]),
  Reason.

% No change planned. The function is there for the behaviour, but will not be used.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.