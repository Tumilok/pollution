defmodule PollutionDataStream do
  @moduledoc false

  def loadData() do
    :pollution_sup.start_link()
    data = importLinesFromCSV() |> Stream.map(&parseLine/1)
    stations = identifyStations(data)

    IO.puts("Loading stations time: #{getTime(fn -> loadStations(stations) end)}s")
    IO.puts("Loading measurements time: #{getTime(fn -> loadValues(data) end)}s")
    IO.puts("Station mean time: #{getTime(fn -> :pollution_gen_server.getStationMean({20.06, 49.986}, 'PM10') end)}s")
    IO.puts("Daily mean time: #{getTime(fn -> :pollution_gen_server.getDailyMean('PM10', {2017, 5, 3}) end)}s")
    IO.puts("Station mean value: #{:pollution_gen_server.getStationMean({20.06, 49.986}, 'PM10')}")
    IO.puts("Daily mean value: #{:pollution_gen_server.getDailyMean('PM10', {2017, 5, 3})}")
  end

  defp loadStations(data) do
    data |> Stream.map(fn {latitude,longitude} -> ['station_#{latitude}_#{longitude}',{latitude,longitude}] end)
    |> Enum.each(fn [name, location] -> :pollution_gen_server.addStation(name,location) end)
  end

  defp loadValues(data) do
    data |> Stream.map(fn x -> {x.location, x.datetime, 'PM10', x.pollutionLevel} end)
    |> Enum.each(fn {location,datetime,type,value} -> :pollution_gen_server.addValue(location,datetime,type,value)end )
  end

  defp importLinesFromCSV() do
    File.stream!("../res/pollution.csv")
  end

  defp parseLine(line) do
    [date, time, latitude, longitude, value] = String.split(line, ",")
    %{
      :datetime => parseDatetime(date, time),
      :location => parseLocation(latitude, longitude),
      :pollutionLevel => parseValue(value)
    }
  end

  defp parseDatetime(date, time) do
    date = String.split(date, "-") |> Enum.reverse |> Stream.map(&(Integer.parse(&1) |> elem(0)))
           |> Enum.reduce({}, fn(element, tuple) -> Tuple.append(tuple, element) end)
    {h, m} = String.split(time, ":") |> Stream.map(&(Integer.parse(&1) |> elem(0)))
           |> Enum.reduce({}, fn(element, tuple) -> Tuple.append(tuple, element) end)
    {date, {h, m, 0}}
  end

  defp parseLocation(latitude, longitude) do
    {Float.parse(latitude) |> elem(0), Float.parse(longitude) |> elem(0)}
  end

  defp parseValue(value) do
    Integer.parse(value) |> elem(0)
  end

  defp identifyStations(data) do
    data |> Stream.map(fn x -> x.location  end)
    |> Stream.uniq() |> Enum.map(fn x -> x  end)
  end

  defp getTime(fun) do
    fun |> :timer.tc |> elem(0)
    |> Kernel./(1_000_000)
  end
end
