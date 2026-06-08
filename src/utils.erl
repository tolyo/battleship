-module(utils).
-export([
    get_random_binary/2,
    update_list_at/3,
    get_template/1
]).

-spec get_random_binary(T, T) -> T.
%% @doc Return one of two values with equal probability.
%% Sample usage: `utils:get_random_binary(<<"p1">>, <<"p2">>).`
get_random_binary(Val1, Val2) ->
    case rand:uniform(2) of
        1 -> Val1;
        2 -> Val2
    end.

-spec update_list_at([term(), ...], pos_integer(), term()) -> [term(), ...].
%% @doc Replace the 1-based list element at an index.
%% Sample usage: `utils:update_list_at([a,b,c], 2, x).`
update_list_at([_ | T], 1, V) ->
    [V | T];
update_list_at([H | T], I, V) when I > 1 ->
    [H | update_list_at(T, I - 1, V)].

compile_once(Path) ->
    {ok, Bin} = file:read_file(Path),
    persistent_term:put({template, Path}, Bin),
    Bin.

-spec get_template(file:filename_all()) -> binary().
%% @doc Read and cache an HTML template, recompiling each call in dev mode.
%% Sample usage: `utils:get_template("priv/static/login/login.html").`
get_template(Path) ->
    case config:is_dev() of
        true ->
            compile_once(Path);
        _ ->
            case persistent_term:get({template, Path}, undefined) of
                undefined ->
                    compile_once(Path);
                Bin ->
                    Bin
            end
    end.
