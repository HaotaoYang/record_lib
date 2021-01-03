-module(record_lib).

-export([
    map_2_record/2,
    record_2_map/1
]).

map_2_record(RecordName, Map) ->
    case record_helper:get_record(RecordName) of
        undefined -> undefined;
        Record ->
            Values = tuple_to_list(erlang:delete_element(1, Record)),
            MapKeys = maps:keys(Map),
            MapValues = maps:values(Map),
            RecordFields = record_helper:fields_info(RecordName),
            FieldsValue = [
                begin
                    case get_field_index(Field, MapKeys) of
                        N when is_integer(N) ->
                            lists:nth(N, MapValues);
                        _ ->
                            lists:nth(Index, Values)
                    end
                end || {Index, Field} <- lists:zip(lists:seq(1, length(RecordFields)), RecordFields)
            ],
            list_to_tuple([RecordName | FieldsValue])
    end.

get_field_index(Field, Keys) ->
    do_get_field_index(Field, Keys, 1).

do_get_field_index(_Field, Keys, N) when N > length(Keys) -> undefined;
do_get_field_index(Field, Keys, N) ->
    case lists:nth(N, Keys) of
        Field -> N;
        _ -> do_get_field_index(Field, Keys, N + 1)
    end.

record_2_map(Record) ->
    RecordName = element(1, Record),
    Fields = record_helper:fields_info(RecordName),
    Values = tuple_to_list(erlang:delete_element(1, Record)),
    {RetMap, _} = lists:foldl(
        fun(Field, {TempMap, Index}) ->
            NewMap = maps:put(Field, lists:nth(Index, Values), TempMap),
            {NewMap, Index + 1}
        end,
        {#{}, 1},
        Fields
    ),
    RetMap.
