-module(redrum).

%% API exports
-export([main/1]).
-compile(export_all).

-define(REBAR_CONF, "rebar.config").
-define(SCHEME_DEFAULTS, [{git, 9418}, {https, 443}]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    {Config, Directory, Opts} = parse_args(Args),
    remap(Config, Directory, Opts),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
parse_args(_Args) ->
    {ok, ok, ok}.

remap(Config, Directory, Opts) ->
    {ok, Files} = file:list_dir(Directory),
    Repos = [filename:join(Directory, File) || File <- Files],
    rpc:pmap({?MODULE, remap_repo}, [Config, Opts], Repos).

remap_repo(Repo, Config, Opts) ->
    io:format("~p remapping ~p~n", [self(), Repo]),
    case is_repo(Repo) of
        true ->
            Res = remap_rebar_config(Repo, Config, Opts),
            io:format("~p remapped ~p ::: ~p~n", [self(), Repo, Res]);
        false ->
            %% not a repo, report?
            io:format("~p  ~p IS NOT A REPO~n", [self(), Repo]),
            {Repo, not_a_repo}
    end.

remap_rebar_config(Repo, Config, Opts) ->
    RebarConfig = filename:join(Repo, ?REBAR_CONF),
    {ok, Terms} = file:consult(RebarConfig),
    Deps = proplists:get_value(deps, Terms, []),
    NewDeps = remap_deps(Deps, Config),
    rewrite_config(NewDeps, Repo, Terms, Opts).

is_repo(Repo) ->
    filelib:is_dir(Repo) andalso
        filelib:is_regular(filename:join([Repo, ?REBAR_CONF])).

remap_deps(Deps, Config) ->
    lists:map(fun(Dep) ->
                      remap_dep(Dep, Config)
              end,
              Deps).

remap_dep(Dep, Config) ->
    Source = element(3, Dep),
    URL = element(2, Source),
    SchemeDefaults = proplists:get_value(scheme_defaults, Config, ?SCHEME_DEFAULTS),
    {ok, URI} = http_uri:parse(URL, [{scheme_defaults, SchemeDefaults}]),
    DepMap = get_dep_mapping(element(1, Dep), Config),
    NewURI = remap_uri(URI, DepMap),
    NewSource = setelement(2, Source, NewURI),
    setelement(3, Dep, NewSource).

get_dep_mapping(Dep, Config) ->
    DepsMap = proplists:get_value(deps, Config, []),
    Defaults = proplists:get_value(default, Config, []),
    case proplists:get_value(Dep, DepsMap) of
        undefined ->
            Defaults;
        L ->
            orddict:merge(fun(_Key, DepMapping, _DefaultMapping) ->
                                  DepMapping
                          end,
                          L, Defaults)
    end.

remap_uri({Scheme, UserInfo, Host, Port, Path, Query}, DepMap) ->
    %% there will always be a value, if just itself
    NewScheme = remap_scheme(Scheme, DepMap),
    NewUserInfo = remap_userinfo(UserInfo, DepMap),
    NewHost = remap_host(Host, DepMap),
    NewPort = remap_port(Port, DepMap),
    NewPath = remap_path(Path, DepMap),
    NewQuery = remap_query(Query, DepMap),
    uri_to_string({NewScheme, NewUserInfo, NewHost, NewPort, NewPath, NewQuery}).

rewrite_config(NewDeps, Repo, Terms, Opts) ->
    {NewDeps, Repo, Terms, Opts}.

remap_scheme(Scheme, DepMap) ->
    remap_simple(scheme, Scheme, DepMap).

remap_userinfo(UserInfo, DepMap) ->
    remap_simple(userinfo, UserInfo, DepMap).

remap_host(Host, DepMap) ->
    remap_simple(host, Host, DepMap).

remap_port(Port, DepMap) ->
    remap_simple(port, Port, DepMap).

remap_path(Path, DepMap) ->
    case proplists:get_value(path, DepMap) of
        L when is_list(L) ->
            PathTokens = string:tokens(Path, "/"),
            {RepoPath, Repo} = lists:split(length(PathTokens)-1 , PathTokens),
            NewPathTokens = lists:foldr(fun(PathToken, Acc) ->
                                                case proplists:get_value(PathToken, L) of
                                                    undefined -> PathToken;
                                                    PT2 -> [PT2 | Acc]
                                                end
                                        end,
                                        [],
                                        RepoPath),
            io:format("new path tokens ~p repo ~p~n", [NewPathTokens, Repo]),
            filename:join(["/", NewPathTokens, Repo]);
        undefined ->
            Path
    end.

remap_query(Query, DepMap) ->
    remap_simple(query_str, Query, DepMap).

remap_simple(URIPartName, FromVal, DepMap) ->
    case proplists:get_value(URIPartName, DepMap) of
        {FromVal, ToVal} ->
            ToVal;
        _ ->
            FromVal
    end.

uri_to_string(URI) ->
    uri:to_string(URI).

test_conf() ->
    [{default,
      [{scheme, {git, https}},
       {host, {"github.com", "gitlab.uk"}},
       {path, [{"basho", "bet365"}]},
       {port, {443, 890}}]
      }].

