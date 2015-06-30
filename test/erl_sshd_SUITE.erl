%%%=============================================================================
%%% @copyright (C) 2015, Erlang Solutions Ltd
%%% @author Marc Sugiyama <marc.sugiyama@erlang-solutions.com>
%%% @doc erl_sshd test
%%% @end
%%%=============================================================================
-module(erl_sshd_SUITE).
-copyright("2015, Erlang Solutions Ltd.").

%% Note: This directive should only be used in test suites.
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 15570).
-define(USERNAME, "username").
-define(PASSWORD, "password").

% create keys locally before running tests

%%%=============================================================================
%%% Callbacks
%%%=============================================================================

suite() ->
    [{timetrap,{minutes,10}}].

init_per_suite(Config) ->
    start_applications(),
    case is_erl_sshd_running() of
        false ->
            ct:pal(Reason = "sshd server is not running"),
            {skip, Reason};
        true ->
            Config
    end.

end_per_testcase(_,_) ->
    ok.

all() ->
    [can_connect].

%%%=============================================================================
%%% Testcases
%%%=============================================================================

can_connect(_Config) ->
    %% GIVEN

    %% WHEN
    {ok, Connection} = ssh:connect("127.0.0.1", ?PORT,
                                   [{silently_accept_hosts, true},
                                    {user_interaction, false},
                                    {user, ?USERNAME},
                                    {password, ?PASSWORD}]),

    %% THEN
    ok = ssh:close(Connection).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

start_applications() ->
    ok = application:load(erl_sshd),
    ok = set_env(port, ?PORT),
    ok = set_env(app, erl_sshd),
    ok = set_env(passwords, [{?USERNAME,?PASSWORD}]),
    application:ensure_all_started(erl_sshd).

is_erl_sshd_running() ->
    proplists:is_defined(erl_sshd, application:which_applications()).

set_env(Key, Value) ->
    ok = application:set_env(erl_sshd, Key, Value).
