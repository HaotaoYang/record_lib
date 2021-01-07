-module(record_lib).

-define(RECORD_UTIL_MOD, get_record_util_opts()).

-export([
    map_2_record/2,
    record_2_map/1
]).

%%% ============================================================================
%%% API
%%% ============================================================================
map_2_record(Map, RecordName) when is_map(Map) andalso is_atom(RecordName) ->
    case get_record(RecordName) of
        undefined -> undefined;
        Record -> do_map_2_record(Map, Record)
    end;
map_2_record(Map, Record) when is_map(Map) andalso is_tuple(Record) andalso Record =/= {} ->
    RecordName = element(1, Record),
    case is_record(Record, RecordName) andalso is_record(RecordName) of
        true -> do_map_2_record(Map, Record);
        _ -> undefined
    end;
map_2_record(_, _) -> undefined.

do_map_2_record(Map, Record) ->
    RecordName = element(1, Record),
    Values = tuple_to_list(erlang:delete_element(1, Record)),
    MapKeys = maps:keys(Map),
    MapValues = maps:values(Map),
    RecordFields = fields_info(RecordName),
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
    list_to_tuple([RecordName | FieldsValue]).

get_field_index(Field, Keys) ->
    do_get_field_index(Field, Keys, 1).

do_get_field_index(_Field, Keys, N) when N > length(Keys) -> undefined;
do_get_field_index(Field, Keys, N) ->
    case lists:nth(N, Keys) of
        Field -> N;
        _ -> do_get_field_index(Field, Keys, N + 1)
    end.

record_2_map(Record) when is_tuple(Record) andalso Record =/= {} ->
    RecordName = element(1, Record),
    case is_record(Record, RecordName) andalso is_record(RecordName) of
        true -> do_record_2_map(RecordName, erlang:delete_element(1, Record));
        _ -> undefined
    end;
record_2_map(_) -> undefined.

do_record_2_map(RecordName, Values) ->
    Fields = fields_info(RecordName),
    {RetMap, _} = lists:foldl(
        fun(Field, {TempMap, Index}) ->
            NewMap = maps:put(Field, lists:nth(Index, tuple_to_list(Values)), TempMap),
            {NewMap, Index + 1}
        end,
        {#{}, 1},
        Fields
    ),
    RetMap.

%%% ============================================================================
%%% INTERNAL API
%%% ============================================================================
get_record_util_opts() ->
    Config = rebar_config:consult_root(),
    Opts = proplists:get_value(record_util_opts, Config, []),
    case lists:keyfind(module_name, 1, Opts) of
        {module_name, ModuleName} when is_list(ModuleName) -> list_to_atom(ModuleName);
        {module_name, ModuleName} when is_atom(ModuleName) -> ModuleName;
        _ -> record_helper
    end.

is_record(RecordName) ->
    erlang:apply(?RECORD_UTIL_MOD, is_record, [RecordName]).

get_record(RecordName) ->
    erlang:apply(?RECORD_UTIL_MOD, get_record, [RecordName]).

fields_info(RecordName) ->
    erlang:apply(?RECORD_UTIL_MOD, fields_info, [RecordName]).
