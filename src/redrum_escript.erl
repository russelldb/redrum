-module(redrum_escript).

%% API exports
-export([main/1]).
-compile(export_all).

-define(REBAR_CONF, "rebar.config").
-define(SCHEME_DEFAULTS, [{git, 9418}, {https, 443}, {ssh, 22}]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ParsedArgs, ExtraArgs} = case getopt:parse(cli_opts(), Args) of
                                  {ok, {P, E}} -> {P, E};
                                  _ -> usage()
                              end,
    io:format("Parsed Args ~p Opts ~p ~n", [ParsedArgs, ExtraArgs]),

    case run_usage(ParsedArgs) of
        true -> usage();
        _ -> ok
    end,

    Conf = read_conf(proplists:get_value(config, ParsedArgs, undefined), ParsedArgs),

    ok = maybe_remap_file(ParsedArgs, Conf),
    maybe_remap_deps(ParsedArgs, Conf),

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
cli_opts() ->
    [
     {file,         $f,        "file",      {string, "rebar.config"}, "File to remap"},
     {skip,         $s,        "skip",      undefined, "skip top level rebar config file"},
     {deps,         $d,        "deps",      string, "remap rebar deps directory"},
     {config,       $c,        "config",    {string, "redrum.conf"},   "config file containing remappings"},
     {help,         $h,        "help",     undefined,                 "help / usage"}
    ].

usage() ->
    getopt:usage(cli_opts(), "redrum"),
    halt(0).

run_usage([]) -> true;
run_usage(ParsedArgs) ->
    lists:member(help, ParsedArgs).

read_conf(undefined, Args) ->
    run_usage(Args);
read_conf(File, _Args) ->
    case file:consult(File) of
        {ok, Conf} ->
            Conf;
        Error ->
            io:format("Error ~p reading config file ~p~n", [Error, File]),
            halt(0)
    end.

%% Only remap a file if asked to
maybe_remap_file(Args, Conf) ->
    Skip = proplists:get_value(skip, Args, false),
    File = proplists:get_value(file, Args),
    maybe_remap_file(Skip, File, Conf).

maybe_remap_file(true, _, _Conf) ->
    ok;
maybe_remap_file(false, File, Conf) ->
    redrum:remap_rebar_config(File, Conf, []).

%% @TODO(implement)
maybe_remap_deps(Args, Conf) ->
    case proplists:get_value(deps, Args, undefined) of
        undefined ->
            ok;
        Dir ->
            redrum:remap(Conf, Dir, [])
    end.

