-module(weather).
-export([forecast/1, accumulator/3]).

forecast(CityList) ->
  % Build a map of city names with empty condition values
  CityCondMap = lists:map(fun(C) -> {C, null} end, CityList),

  % Spawn an accumulator process to capture async results as they come in
  AccumulatorPid = spawn(weather, accumulator, [self(), CityList, CityCondMap]),

  % For each item in the list, launch a Process which will call the weather_api:get_weather/1 function
  lists:foreach(fun(City) ->
    spawn(fun() ->
      % Break out the results of the weather_api call so that less information is sent over the event bus
      {weather, {current, _, _}, {forecast, _, Cond}} = weather_api:get_weather(City),
      AccumulatorPid ! {result, City, Cond}
    end)
  end, CityList),

  % Await all results to come in from the accumulator
  receive
    {done, CondList} ->
      {_, Conditions} = lists:unzip(CondList),
      Conditions
  end.


%% @doc Listen for messages from async_weather/2 and accumulate them until all specified Cities have been accounted for
accumulator(ParentPid, [], State) ->
  ParentPid ! {done, State};
accumulator(ParentPid, CityList, State) ->
  receive
    {result, City, Cond} ->
      NewCityList = lists:delete(City, CityList),
      accumulator(ParentPid, NewCityList, lists:keyreplace(City, 1, State, {City, Cond}))
  end.