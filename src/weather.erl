-module(weather).
-export([forecast/1, accumulator/3]).

forecast(CityList) ->
  % Spawn an accumulator Process
  CityCondMap = lists:map(fun(C) -> {C, null} end, CityList),
  AccumulatorPid = spawn(weather, accumulator, [self(), CityList, CityCondMap]),

  % For each item in the list, launch a Process which will call the weather_api:get_weather/1 function
  lists:foreach(fun(City) -> spawn(fun() -> AccumulatorPid ! {City, weather_api:get_weather(City)} end) end, CityList),
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
    {City, {weather, {current, _, _}, {forecast, _, Cond}}} ->
      NewCityList = lists:delete(City, CityList),
      accumulator(ParentPid, NewCityList, lists:keyreplace(City, 1, State, {City, Cond}))
  end.