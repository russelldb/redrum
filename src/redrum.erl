-module(redrum).

%% API exports
-compile(export_all).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(REBAR_CONF, "rebar.config").
-define(SCHEME_DEFAULTS, [{git, 9418}, {https, 443}, {ssh, 22}]).

%%====================================================================
%% API functions
%%====================================================================

remap(Config, Directory, Opts) ->
    {ok, Files} = file:list_dir(Directory),
    Repos = [filename:join(Directory, File) || File <- Files],
    rpc:pmap({?MODULE, remap_repo}, [Config, Opts], Repos).

remap_repo(Repo, Config, Opts) ->
    io:format("~p remapping ~p~n", [self(), Repo]),
    case is_repo(Repo) of
        true ->
            RebarConfig = filename:join(Repo, ?REBAR_CONF),
            Res = remap_rebar_config(RebarConfig, Config, Opts),
            io:format("~p remapped ~p ::: ~p~n", [self(), Repo, Res]);
        false ->
            %% not a repo, report?
            io:format("~p  ~p has no rebar.config~n", [self(), Repo]),
            {Repo, not_a_repo}
    end.

remap_rebar_config(RebarConfigFile, Config, Opts) ->
    {ok, Terms} = file:consult(RebarConfigFile),
    Deps = proplists:get_value(deps, Terms, []),
    NewDeps = remap_deps(Deps, Config),
    %% TODO =: this is probably not good enough, we probably need to
    %% parse into AST and manipulate that, retaining any comments
    rewrite_config(NewDeps, RebarConfigFile, Terms, Opts).

is_repo(Repo) ->
    filelib:is_dir(Repo) andalso
        filelib:is_regular(filename:join([Repo, ?REBAR_CONF])).

remap_deps(Deps, Config) ->
    lists:map(fun(Dep) ->
                      remap_dep(Dep, Config)
              end,
              Deps).

remap_dep(Dep, Config) ->
    io:format("remapping dep ~p~n", [Dep]),
    {_Name, _Vsn, Source} = Dep,
    {Engine, URL, Rev} = Source,
    DepMapping = get_dep_mapping(Dep, Config),
    {ok, URI} = parse_url(URL, Config),
    NewURI = remap_uri(URI, DepMapping, Config),
    NewEngine = remap_engine(Engine, DepMapping),
    NewRev = remap_rev(Rev, DepMapping),
    io:format("new uri ~p~n", [NewURI]),
    setelement(3, Dep, {NewEngine, NewURI, NewRev}).

remap_rev(Rev, Mapping) ->
    remap_simple(rev, Rev, Mapping).

remap_engine(Engine, Mapping) ->
    remap_simple(engine, Engine, Mapping).

get_dep_mapping(Dep, Config) ->
    try
        lists:foldl(fun(MappingFun, undefined) ->
                            io:format("trying ~p~n", [MappingFun]),
                            erlang:apply(?MODULE, MappingFun, [Dep, Config]);
                       (_MappingFun, Mapping) ->
                            throw({found, Mapping})
                    end, undefined, [name_mapping, engine_mapping, host_mapping, path_mapping, default_mapping, no_mapping])
    catch {found, Mapping} ->
            Mapping
    end.


%% @doc pulls a specific mapping config for this dep based on it's
%% name in rebar.config
name_mapping({Name, _Vsn, _Source}=_Dep, Config) ->
    DepsMap = proplists:get_value(deps, Config),
    proplists:get_value(Name, DepsMap).

%% @doc engine mapping: any dep who's engine has an entry in the
%% engine_map
engine_mapping({_Name, _Vsn, {Engine, _URL, _Rev}=_Source}, Config) ->
    EngineMap = proplists:get_value(engines, Config, []),
    proplists:get_value(Engine, EngineMap).

%% @doc host mapping, remap any dep that is at a matched host
host_mapping({_Name, _Vsn, {_Engine, URL, _Rev}=_Source}, Config) ->
    HostMap = proplists:get_value(hosts, Config, []),
    {ok, URI} = parse_url(URL, Config),
    Host = get_host(URI),
    proplists:get_value(Host, HostMap).

%% @doc path mapping: at the moment v.simple, assumes githubesque
%% urls, and path is the bit between host and query, so user is the
%% bit between host and /
path_mapping({_Nam, _Vsn, {_Engine, URL, _Rev}=_Source}, Config) ->
    PathMap = proplists:get_value(paths, Config, []),
    {ok, URI} = parse_url(URL, Config),
    Path = get_path(URI),
    PathTokens = string:tokens(Path, "/"),
    proplists:get_value(hd(PathTokens), PathMap).

default_mapping(_Dep, Config) ->
    proplists:get_value(default_mapping, Config).

no_mapping(_Dep, _Config) ->
    [].

get_host({_Scheme, _UserInfo, Host, _Port, _Path, _Query}) ->
    Host.

get_path({_Scheme, _UserInfo, _Host, _Port, Path, _Query}) ->
    Path.

remap_uri({Scheme, UserInfo, Host, Port, Path, Query}, DepMap, Config) ->
    %% there will always be a value, if just itself
    NewScheme = remap_scheme(Scheme, DepMap),
    NewUserInfo = remap_userinfo(UserInfo, DepMap),
    NewHost = remap_host(Host, DepMap),
    NewPort = remap_port(Port, DepMap),
    NewPath = remap_path(Path, DepMap),
    NewQuery = remap_query(Query, DepMap),
    uri_to_string({NewScheme, NewUserInfo, NewHost, NewPort, NewPath, NewQuery}, Config).

rewrite_config(NewDeps, RebarConfigFile, Terms, _Opts) ->
    NewTerms = lists:keystore(deps, 1, Terms, {deps,  NewDeps}),
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = lists:map(Format, NewTerms),
    replace_file(RebarConfigFile, Text).

remap_scheme(Scheme, DepMap) ->
    remap_simple(scheme, Scheme, DepMap).

remap_userinfo(UserInfo, DepMap) ->
    remap_simple(userinfo, UserInfo, DepMap).

remap_host(Host, DepMap) ->
    remap_simple(host, Host, DepMap).

remap_port(Port, DepMap) ->
    remap_simple(port, Port, DepMap).

remap_path(Path, DepMap) ->
    {RepoPath, Repo} = {filename:dirname(Path), filename:basename(Path)},
    NewRepoPath = remap_simple(path, RepoPath, DepMap),
    filename:join(["/", NewRepoPath, Repo]).

remap_query(Query, DepMap) ->
    remap_simple(query_str, Query, DepMap).

remap_simple(URIPartName, FromVal, DepMap) ->
    case proplists:get_value(URIPartName, DepMap) of
        {'*', ToVal} ->
            ToVal;
        {FromVal, ToVal} -> ToVal;
        L when is_list(L) ->
            case proplists:get_value(FromVal, L) of
                undefined -> FromVal;
                ToVal -> ToVal
            end;
        _ ->
            FromVal
    end.

uri_to_string(URI, Config) ->
    SchemeDefaults = get_scheme_defaults(Config),
    uri_to_string(URI, SchemeDefaults, proplists:get_value(scp_style, Config, true)).

uri_to_string(URI, SchemeDefaults, true) when is_tuple(URI) andalso element(1, URI) == ssh ->
    ssh_url_to_scp(URI, SchemeDefaults);
uri_to_string(URI, SchemeDefaults, _SSHToSCP) ->
    uri:to_string(URI, SchemeDefaults).

%% @doc Atomically/safely (to some reasonable level of durablity)
%% replace file `FN' with `Data'.
-spec replace_file(string(), iodata()) -> ok | {error, term()}.
replace_file(FN, Data) ->
    TmpFN = FN ++ ".tmp",
    case file:open(TmpFN, [write, raw]) of
        {ok, FH} ->
            try
                ok = file:write(FH, Data),
                ok = file:sync(FH),
                ok = file:close(FH),
                ok = file:rename(TmpFN, FN),
                {ok, Contents} = read_file(FN),
                true = (Contents == iolist_to_binary(Data)),
                ok
            catch _:Err ->
                    {error, Err}
            end;
        Err ->
            Err
    end.

%% @doc Similar to {@link file:read_file/1} but uses raw file `I/O'
read_file(FName) ->
    {ok, FD} = file:open(FName, [read, raw, binary]),
    IOList = read_file(FD, []),
    ok = file:close(FD),
    {ok, iolist_to_binary(IOList)}.

read_file(FD, Acc) ->
    case file:read(FD, 4096) of
        {ok, Data} ->
            read_file(FD, [Data|Acc]);
        eof ->
            lists:reverse(Acc)
    end.

parse_url(URL, Config) ->
    SchemeDefaults = get_scheme_defaults(Config),
    case http_uri:parse(URL, SchemeDefaults) of
        {ok, URI} ->
            {ok, URI};
        {error, {malformed_url, _, _}} ->
            maybe_parse_scp_style_url(URL, Config)
    end.

get_scheme_defaults(Config) ->
    SchemeDefaults = proplists:get_value(scheme_defaults, Config, ?SCHEME_DEFAULTS),
    [{scheme_defaults, SchemeDefaults}].

%% honestly a hack, I don't know if it is good enough
maybe_parse_scp_style_url(URL, Config) ->
    Tokens = string:tokens(URL, "@:"),
    case Tokens of
        [User, HostPath] ->
            NewURL = lists:flatten(["ssh://", User, "@", HostPath]),
            parse_url(NewURL, Config);
        [User, Host, Path] ->
            NewURL = lists:flatten(["ssh://", User, "@", Host, "/", Path]),
            parse_url(NewURL, Config)
    end.

%% as above, a quick and dirty hack
ssh_url_to_scp({ssh, UserInfo, Host, Port, Path, Query}=URI, SchemeDefaults0) ->
    [{_, SchemeDefaults}] = SchemeDefaults0,
    case proplists:get_value(ssh, SchemeDefaults) of
        Port ->  lists:flatten([UserInfo, "@", Host, ":", Path, Query]);
        _Int ->
            %% don't know how to add port info to scp style url, so stick to SSH style
            uri_to_string(URI, SchemeDefaults0, false)
    end.

%%% just for console testing/faffing
test_conf() ->
    [{default,
      [{scheme, {git, https}},
       {host, {"github.com", "gitlab.uk"}},
       {path, [{"basho", "bet365"}]},
       {port, {443, 890}}]
      }].


-ifdef(TEST).
-endif.
