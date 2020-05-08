%%%-------------------------------------------------------------------
%%% @author tumilok
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Mar 2020 16:17
%%%-------------------------------------------------------------------
-module(pollution).
-author("tumilok").

%% API
-export([
  createMonitor/0,
  addStation/3,
  addValue/5,
  removeValue/4,
  getOneValue/4,
  getStationMean/3,
  getDailyMean/3,
  getHourlyMean/3,
  getDailyAverageDataCount/3,
  getDailyOverLimit/4,
  getMaximumGradientStations/2,
  getMinValue/2,
  getMaxValue/2
]).

%  record contains the information about station
-record(station, {coords, name}).
% record contains the information about measurement
-record(measurement, {type, date}).

% function creates new monitor
createMonitor() -> maps:new().

% function uses an argument key to find the full key of data
getStationKey([], _) -> null;
getStationKey([#station{name=Key, coords=Coords}|_], Key) -> #station{name=Key, coords=Coords};
getStationKey([#station{name=Name, coords=Key}|_], Key) -> #station{name=Name, coords=Key};
getStationKey([_|T], Key) -> getStationKey(T, Key).

% function adds new station to the given monitor
addStation(Name, _, _) when not is_list(Name) -> {error, "Name must be a list"};
addStation(_, Coords, _) when not is_tuple(Coords) -> {error, "Coordinates must be a tuple"};
addStation(Name, Coords, Monitor) ->
  NameExists = getStationKey(maps:keys(Monitor), Name),
  CoordsExists = getStationKey(maps:keys(Monitor), Coords),
  case {NameExists, CoordsExists} of
    {null, null} -> Monitor#{#station{name=Name, coords=Coords} => maps:new()};
    {null, _} -> {error, "Coordinates of such a value are already exist"};
    {_, _} -> {error, "Name of such a value is already exist"}
  end.

% function finds station with the help of key
% and adds given measurement to it
addValue(_, Date, _, _, _) when not is_tuple(Date) -> {error, "Date must be a tuple"};
addValue(_, _, Type, _, _) when not is_list(Type) -> {error, "Type must be a list"};
addValue(_, _, _, Val, _) when not (is_integer(Val) or is_float(Val)) -> {error, "Value must be a number"};
addValue(_, _, _, _, Monitor) when Monitor == #{} -> {error, "Monitor is empty"};
addValue(Key, Date, Type, Val, Monitor) ->
  Value = maps:find(getStationKey(maps:keys(Monitor), Key), Monitor),
  case Value of
    error -> io:format("Station doesn't exist ~n");
    {ok, Measurement} ->
      Exists = maps:find(#measurement{type=Type, date=Date}, Measurement),
      case Exists of
        error ->
          case maps:size(Measurement) of
            0 -> maps:put(getStationKey(maps:keys(Monitor), Key), maps:put(#measurement{type=Type, date=Date}, Val, Measurement), Monitor);
            _ -> maps:update(getStationKey(maps:keys(Monitor), Key), maps:put(#measurement{type=Type, date=Date}, Val, Measurement), Monitor)
          end;
        _ -> {error, "Type and Date of such a value are already exist"}
      end
  end.

% function finds measurement with the help of provided information
% and removes it from the station map
removeValue(_, Date, _, _) when not is_tuple(Date) -> {error, "Date must be a tuple"};
removeValue(_, _, Type, _) when not is_list(Type) -> {error, "Type must be a list"};
removeValue(_, _, _, Monitor) when Monitor == #{} -> {error, "Monitor is empty"};
removeValue(Key, Date, Type, Monitor) ->
  Value = maps:find(getStationKey(maps:keys(Monitor), Key), Monitor),
  case Value of
    error -> {error, "Station doesn't exist"};
    {ok, Measurement} ->
      ToRemove = maps:find(#measurement{type=Type, date=Date}, Measurement),
      case ToRemove of
        error -> {error, "Measurments with such parameters don't exist"};
        _ -> NewMeasurement = maps:remove(#measurement{type=Type, date=Date}, Measurement),
          maps:update(getStationKey(maps:keys(Monitor), Key), NewMeasurement, Monitor)
      end
  end.

% function finds measurement with the help of provided information
% and returns it's value
getOneValue(_, Date, _, _) when not is_tuple(Date) -> {error, "Date must be a tuple"};
getOneValue(_, _, Type, _) when not is_list(Type) -> {error, "Type must be a list"};
getOneValue(_, _, _, Monitor) when Monitor == #{} -> {error, "Monitor is empty"};
getOneValue(Key, Date, Type, Monitor) ->
  Value = maps:find(getStationKey(maps:keys(Monitor), Key), Monitor),
  case Value of
    error -> {error, "Station doesn't exist"};
    {ok, Measurement} ->
      ToReturn = maps:find(#measurement{type=Type, date=Date}, Measurement),
      case ToReturn of
        error -> {error, "Measurments with such parameters don't exist"};
        {ok, Val} -> Val
      end
  end.

% function finds station with the help of provided key
% and runs getStationMeanByType function
getStationMean(_, Type, _) when not is_list(Type) -> {error, "Type must be a list"};
getStationMean(_, _, Monitor) when Monitor == #{} -> {error, "Monitor is empty"};
getStationMean(Key, Type, Monitor) ->
  Value = maps:find(getStationKey(maps:keys(Monitor), Key), Monitor),
  case Value of
    error -> {error, "Station doesn't exist"};
    {ok, Measurement} ->
      getStationMeanByType(maps:to_list(Measurement), Type, 0, 0)
  end.

% function iterates measurements, checks its
% type and returns actual mean of all suitable values
getStationMeanByType([], _, _, 0) -> 0.0;
getStationMeanByType([], _, Sum, Size) -> Sum / Size;
getStationMeanByType([{#measurement{type=Type}, Val}|T], Type, Sum, Size) ->
  getStationMeanByType(T, Type, Sum+Val, Size+1);
getStationMeanByType([_|T], Type, Sum, Size) ->
  getStationMeanByType(T, Type, Sum, Size).

% function iterates all stations,
% runs given function and returns mean
iterateStationsMean(_, [], _, _, _, _, 0) -> 0.0;
iterateStationsMean(_, [], _, _, _, Sum, Size) -> Sum / Size;
iterateStationsMean(Monitor, [H|T], Fun, Type, Arg, Sum, Size) ->
  {ok, Val} = maps:find(H, Monitor),
  {MSum, MSize} = Fun(maps:to_list(Val), Type, Arg, {0,0}),
  iterateStationsMean(Monitor, T, Fun, Type, Arg, Sum + MSum, Size + MSize).

% function runs recursive function iterateStationsMean
getDailyMean(Type, _, _) when not is_list(Type) -> {error, "Type must be a list"};
getDailyMean(_, Day, _) when not is_tuple(Day) -> {error, "Day must be a tuple"};
getDailyMean(_, _, Monitor) when Monitor == #{} -> {error, "Monitor is empty"};
getDailyMean(Type, Day, Monitor) ->
  iterateStationsMean(Monitor, maps:keys(Monitor), fun getDailyMeanByType/4, Type, Day, 0, 0).

% function iterates measurements, checks its
% type and date, and returns sum and size of all suitable values
getDailyMeanByType([], _, _, {Sum,Size}) -> {Sum,Size};
getDailyMeanByType([{#measurement{type=Type, date={Day, _}}, Val}|T], Type, Day, {Sum,Size}) ->
  getDailyMeanByType(T, Type, Day, {Sum+Val,Size+1});
getDailyMeanByType([_|T], Type, Day, {Sum,Size}) ->
  getDailyMeanByType(T, Type, Day, {Sum,Size}).

% function runs recursive function iterateStationsMean
getHourlyMean(Type, _, _) when not is_list(Type) -> {error, "Type must be a list"};
getHourlyMean(_, Hour, _) when not is_integer(Hour) -> {error, "Day must be an integer"};
getHourlyMean(_, _, Monitor) when Monitor == #{} -> {error, "Monitor is empty"};
getHourlyMean(Type, Hour, Monitor) ->
  iterateStationsMean(Monitor, maps:keys(Monitor), fun getHourlyMeanByType/4, Type, Hour, 0, 0).

% function iterates measurements, checks its
% type and hour, and returns sum and size of all suitable values
getHourlyMeanByType([], _, _, {Sum,Size}) -> {Sum,Size};
getHourlyMeanByType([{#measurement{type=Type, date={_ ,{Hour,_,_}}}, Val}|T], Type, Hour, {Sum,Size}) ->
  getHourlyMeanByType(T, Type, Hour, {Sum+Val,Size+1});
getHourlyMeanByType([_|T], Type, Hour, {Sum,Size}) ->
  getHourlyMeanByType(T, Type, Hour, {Sum,Size}).

% function runs recursive function finds station and runs countMeasurementsByDay
getDailyAverageDataCount(_, Date, _) when not is_tuple(Date) -> {error, "Date must be a tuple"};
getDailyAverageDataCount(_, _, Monitor) when Monitor == #{} -> {error, "Monitor is empty"};
getDailyAverageDataCount(Key, Date, Monitor) ->
  Value = maps:find(getStationKey(maps:keys(Monitor), Key), Monitor),
  case Value of
    error -> {error, "Station doesn't exist"};
    {ok, Measurement} -> countMeasurementsByDay(maps:to_list(Measurement), Date, 0)
  end.

% function iterates measurements, checks its
% date, and returns amount of all suitable values
countMeasurementsByDay([], _, Count) -> Count;
countMeasurementsByDay([{#measurement{date={Date, _}}, _}|T], Date, Count) ->
  countMeasurementsByDay(T, Date, Count+1);
countMeasurementsByDay([_|T], Date, Count) ->
  countMeasurementsByDay(T, Date, Count).

% function runs recursive function iterateStationsLimit
getDailyOverLimit(Date, _, _, _) when not is_tuple(Date) -> {error, "Date must be a tuple"};
getDailyOverLimit(_, Type, _, _) when not is_list(Type) -> {error, "Type must be a list"};
getDailyOverLimit(_, _, Limit, _) when not (is_integer(Limit) or is_float(Limit)) -> {error, "Limit must be a number"};
getDailyOverLimit(_, _, _, Monitor) when not Monitor == #{} -> {error, "Monitor is empty"};
getDailyOverLimit(Date, Type, Limit, Monitor) ->
  iterateStationsLimit(Monitor, maps:keys(Monitor), Date, Type, Limit, 0).

% function iterates all stations,runs
% isOverLimit function and returns
% number of stations that exceeded limit
iterateStationsLimit(_, [], _, _, _, Count) -> Count;
iterateStationsLimit(Monitor, [H|T], Date, Type, Limit, Count) ->
  {ok, Val} = maps:find(H, Monitor),
  IsTrue = isOverLimit(maps:to_list(Val), Date, Type, Limit),
  case IsTrue of
    true -> iterateStationsLimit(Monitor, T, Date, Type, Limit, Count+1);
    false -> iterateStationsLimit(Monitor, T, Date, Type, Limit, Count)
  end.

% function iterates measurements, checks its
% Type and Date, and returns true if value
% of measurement was greater than limit
isOverLimit([], _, _, _) -> false;
isOverLimit([{#measurement{type=Type, date={Date, _}}, Val}|T], Date, Type, Limit) ->
  case Val > Limit of
    true -> true;
    false -> isOverLimit(T, Date, Type, Limit)
  end;
isOverLimit([_|T], Date, Type, Limit) ->
  isOverLimit(T, Date, Type, Limit).

% function runs recursive function iterateStationsMinMaxDiff
% with getDiff function as argument
getMaximumGradientStations(Type, _) when not is_list(Type) -> {error, "Type must be a list"};
getMaximumGradientStations(_, Monitor) when not Monitor == #{} -> {error, "Monitor is empty"};
getMaximumGradientStations(Type, Monitor) ->
  iterateStationsMinMaxDiff(Monitor, maps:keys(Monitor), fun getDiff/2, Type, 100000, -100000).

% function returns difference of values
getDiff(Min, Max) -> Max - Min.

% function runs recursive function iterateStationsMinMaxDiff
% with getMin function as argument
getMinValue(Type, _) when not is_list(Type) -> {error, "Type must be a list"};
getMinValue(_, Monitor) when not Monitor == #{} -> {error, "Monitor is empty"};
getMinValue(Type, Monitor) ->
  iterateStationsMinMaxDiff(Monitor, maps:keys(Monitor), fun getMin/2, Type, 100000, -100000).

% function returns min value
getMin(Min, _) -> Min.

% function runs recursive function iterateStationsMinMaxDiff
% with getMax function as argument
getMaxValue(Type, _) when not is_list(Type) -> {error, "Type must be a list"};
getMaxValue(_, Monitor) when not Monitor == #{} -> {error, "Monitor is empty"};
getMaxValue(Type, Monitor) ->
  iterateStationsMinMaxDiff(Monitor, maps:keys(Monitor), fun getMax/2, Type, 100000, -100000).

% function returns max value
getMax(_, Max) -> Max.

% function iterates all stations,runs
% getMaximumGradientStationsByType function and returns
% given function with values min and max
iterateStationsMinMaxDiff(_, [], _, _, 100000, -100000) -> error;
iterateStationsMinMaxDiff(_, [], Fun, _, Min, Max) -> Fun(Min, Max);
iterateStationsMinMaxDiff(Monitor, [H|T], Fun, Type, Min, Max) ->
  {ok, Val} = maps:find(H, Monitor),
  {MMin, MMax} = getMaximumGradientStationsByType(maps:to_list(Val), Type, {100000,-100000}),
  case {MMin < Min, MMax > Max} of
    {true, true} -> iterateStationsMinMaxDiff(Monitor, T, Fun, Type, MMin, MMax);
    {true, false} -> iterateStationsMinMaxDiff(Monitor, T, Fun, Type, MMin, Max);
    {false, true} -> iterateStationsMinMaxDiff(Monitor, T, Fun, Type, Min, MMax);
    {false, false} -> iterateStationsMinMaxDiff(Monitor, T, Fun, Type, Min, Max)
  end.

% function iterates measurements, checks its
% Type, and returns min and max value
getMaximumGradientStationsByType([], _, {Min, Max}) -> {Min, Max};
getMaximumGradientStationsByType([{#measurement{type=Type}, Val}|T], Type, {Min, Max}) ->
  case {Val < Min, Val > Max} of
    {true, true} -> getMaximumGradientStationsByType(T, Type, {Val, Val});
    {true, false} -> getMaximumGradientStationsByType(T, Type, {Val, Max});
    {false, true} -> getMaximumGradientStationsByType(T, Type, {Min, Val});
    {false, false} -> getMaximumGradientStationsByType(T, Type, {Min, Max})
  end;
getMaximumGradientStationsByType([_|T], Type, {Min, Max}) ->
  getMaximumGradientStationsByType(T, Type, {Min, Max}).