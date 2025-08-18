-module(battleship_register_handler).
-behaviour(cowboy_rest).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    from_json/2,
    from_register_form/2
]).

%%--------------------------------------------------------------------
%% Cowboy REST callbacks
%%--------------------------------------------------------------------

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.


%% Provide HTML for GET
content_types_provided(Req, State) ->
    {[{<<"text/html">>, from_register_form}], Req, State}.


%%--------------------------------------------------------------------
%% GET handler using ErlyDTL template
%%--------------------------------------------------------------------
from_register_form(Req, State) ->
    compile_once(register_template, "priv/static/register/register.html.dt"),
    Bindings = #{},
    {ok, Html} = register_template:render(Bindings),
    {Html, Req, State}.




%% Compile an ErlyDTL template only once per VM run
-spec compile_once(atom(), string()) -> ok.
compile_once(Module, TemplatePath) when is_atom(Module), is_list(TemplatePath) ->
    case code:is_loaded(Module) of
        false ->
            %% Compile the template only if not already loaded
            erlydtl:compile_file(TemplatePath, Module),
            ok;
        {_Mod, _Path} ->
            case dotenv_config:get(<<"ENV">>) of
              <<"dev">> ->
                    erlydtl:compile_file(TemplatePath, Module),
                    ok;
                _ ->  
                    ok
            end    
    end.

%% Accept JSON body only for POST
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

%%--------------------------------------------------------------------
%% JSON POST handler
%%--------------------------------------------------------------------
from_json(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case json:decode(Body) of
        {ok, Map} ->
            Username = maps:get(<<"username">>, Map),
            Email    = maps:get(<<"email">>, Map),
            Password = maps:get(<<"password">>, Map),

            case battleship_user:create(Username, Email, Password) of
                {ok, UserId} ->
                    Resp = json:encode(#{status => <<"ok">>, user_id => UserId}),
                    Req2 = cowboy_req:reply(201,
                        #{<<"content-type">> => <<"application/json">>},
                        Resp,
                        Req1),
                    {stop, Req2, State};

                {error, Reason} ->
                    Resp = json:encode(#{status => <<"error">>, reason => Reason}),
                    Req2 = cowboy_req:reply(400,
                        #{<<"content-type">> => <<"application/json">>},
                        Resp,
                        Req1),
                    {stop, Req2, State}
            end;
        {error, Reason} ->
            Resp = json:encode(#{status => <<"error">>, reason => Reason}),
            Req2 = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                Resp,
                Req1),
            {stop, Req2, State}
    end.
