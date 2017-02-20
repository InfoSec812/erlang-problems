-module(weather).
-export([forecast/1, accumulator/3, async_weather/2]).

%% *********** WARNING ***********
%% In a REAL application calling a web based API, there is no guarantee that the results will come back in the order
%% in which they are sent... This can be addressed by using a Map instead of a List, or it is also possible to have a
%% list of Tuples and iterate over them and use the source list to order the output.

forecast(CityList) ->
  % Spawn an accumulator Process
  AccumulatorPid = spawn(weather, accumulator, [self(), CityList, []]),

  % For each item in the list, launch a Process which will call the weather_api:get_weather/1 function
  forecast_weather(AccumulatorPid, CityList),
  receive
    {done, CondList} ->
      CondList
  end.

%% @doc Spawn a process to call async_weather/2 for each City in the CityList
forecast_weather(_, []) ->
  ok;
forecast_weather(AccumulatorPid, [Head|Tail]) ->
  spawn(weather, async_weather, [AccumulatorPid, Head]),
  forecast_weather(AccumulatorPid, Tail).

%% @doc Call weather_api:get_weather/1 for the specified City and send the results to AccumulatorPid
async_weather(AccumulatorPid, City) ->
  Weather = weather_api:get_weather(City),
  AccumulatorPid ! {City, Weather}.

%% @doc Listen for messages from async_weather/2 and accumulate them until all specified Cities have been accounted for
accumulator(ParentPid, CityList, State) when length(CityList) == 0 ->
  io:format("Sending results: ~p~n", [State]),
  ParentPid ! {done, State};
accumulator(ParentPid, CityList, State) ->
  receive
    {City, {weather, {current, _, _}, {forecast, _, Cond}}} ->
      NewCityList = lists:delete(City, CityList),
      io:format("City List: ~p~n", [NewCityList]),
      accumulator(ParentPid, NewCityList, State ++ [Cond])
  end.