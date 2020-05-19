defmodule PollutionData do
  @moduledoc false

  def loadData() do
    :pollution_sup.start_link()
    data = importLinesFromCSV() |> Enum.map(&parseLine/1)
    stations = identifyStations(data)

    IO.puts("Loading stations time: #{getTime(fn -> loadStations(stations) end)}s")
    IO.puts("Loading measurements time: #{getTime(fn -> loadValues(data) end)}s")
    IO.puts("Station mean time: #{getTime(fn -> :pollution_gen_server.getStationMean({20.06, 49.986}, 'PM10') end)}s")
    IO.puts("Daily mean time: #{getTime(fn -> :pollution_gen_server.getDailyMean('PM10', {2017, 5, 3}) end)}s")
    IO.puts("Station mean value: #{:pollution_gen_server.getStationMean({20.06, 49.986}, 'PM10')}")
    IO.puts("Daily mean value: #{:pollution_gen_server.getDailyMean('PM10', {2017, 5, 3})}")
  end

  defp loadStations([]) do :ok end
  defp loadStations([h | t]) do
    :pollution_gen_server.addStation(
      'station_#{h.location |> elem(0)}_#{h.location |> elem(1)}', h.location)
    loadStations(t)
  end

  defp loadValues([]) do :ok end
  defp loadValues([h | t]) do
    :pollution_gen_server.addValue(
      'station_#{h.location |> elem(0)}_#{h.location |> elem(1)}',
      h.datetime, 'PM10', h.pollutionLevel)
    loadValues(t)
  end

  defp importLinesFromCSV() do
    File.read!("../res/pollution.csv") |> String.split("\r\n")
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
    date = String.split(date, "-") |> Enum.reverse
           |> Enum.map(&(Integer.parse(&1) |> elem(0))) |> :erlang.list_to_tuple()
    {h, m} = String.split(time, ":")
             |> Enum.map(&(Integer.parse(&1)|> elem(0))) |> :erlang.list_to_tuple()
    {date, {h, m, 0}}
  end

  defp parseLocation(latitude, longitude) do
    {Float.parse(latitude) |> elem(0), Float.parse(longitude) |> elem(0)}
  end

  defp parseValue(value) do
    Integer.parse(value) |> elem(0)
  end

  defp identifyStations(data) do
    Enum.uniq_by(data, fn record -> record.location end)
  end

  defp getTime(fun) do
    fun |> :timer.tc |> elem(0)
    |> Kernel./(1_000_000)
  end
end