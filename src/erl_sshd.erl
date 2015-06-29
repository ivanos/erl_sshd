-module(erl_sshd).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    Passwords = application:get_env(passwords, []),
    Port = application:get_env(erl_sshd, port, 11111),
    PrivDir = code:priv_dir(erl_sshd),
    SystemDir = filename:join([PrivDir, "system_dir"]),
    UserDir = filename:join([PrivDir, "user_dir"]),
    gen_server:cast(self(), start),
    {ok, #{port => Port,
           user_dir => UserDir,
           system_dir => SystemDir,
           passwords => Passwords,
           pid => undefined}}.

handle_call(Request, _From, State) ->
    {stop, {unimplemented, call, Request}, State}.

handle_cast(start, State = #{port := Port,
                             user_dir := UserDir,
                             system_dir := SystemDir,
                             passwords := Passwords}) ->
    {ok, Pid} = ssh:daemon(Port, [{system_dir, SystemDir},
                                  {user_dir, UserDir},
                                  {user_passwords, Passwords}]),
    link(Pid),
    {noreply, State#{pid => Pid}, hibernate};
handle_cast(Msg, State) ->
    {stop, {unimplemented, cast, Msg}, State}.

handle_info(Info, State) ->
    {stop, {unimplemented, info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

