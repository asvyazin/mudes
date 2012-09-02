-module(mudes_users_db).
-author('Alexander Svyazin <guybrush@live.ru>').

-behaviour(gen_server).

%% API
-export([add/2, exists/1, check_password/2, remove/1, create_tables/0]).

%% gen_server API
-export([start_link/0, stop/0]).

%% callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state, {}).
-record(users, {login, password_hash}).

create_tables() ->
    mnesia:create_table(users, [{disc_copies, [node()]},
				{attributes, record_info(fields, users)}]).    

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ok = mnesia:wait_for_tables([users], 60000),
    {ok, #state{}}.

add(Login, Password) ->
    gen_server:call(?MODULE, {add, Login, Password}).

exists(Login) ->
    gen_server:call(?MODULE, {exists, Login}).

check_password(Login, Password) ->
    gen_server:call(?MODULE, {check_password, Login, Password}).

remove(Login) ->
    gen_server:call(?MODULE, {remove, Login}).

stop() ->
    gen_server:cast(?MODULE, stop).

handle_call({add, Login, Password}, _From, State) ->
    PasswordHash = crypto:sha(Password),
    NewUser = #users{login = Login, password_hash = PasswordHash},
    Fun = fun() -> mnesia:write(NewUser) end,
    {atomic, ok} = mnesia:transaction(Fun),
    {reply, ok, State};
handle_call({exists, Login}, _From, State) ->
    Result = case get_user(Login) of
		 {ok, _} ->
		     true;
		 _ ->
		     false
	     end,
    {reply, Result, State};
handle_call({check_password, Login, Password}, _From, State) ->
    Result = case get_user(Login) of
		 {ok, #users{password_hash = ValidHash}} ->
		     case crypto:sha(Password) of
			 ValidHash ->
			     true;
			 _ ->
			     false
		     end;
		 _ ->
		     false
	     end,
    {reply, Result, State};
handle_call({remove, Login}, _From, State) ->
    Fun = fun() -> mnesia:delete({users, Login}) end,
    {atomic, ok} = mnesia:transaction(Fun),
    {reply, ok, State}.

get_user(Login) ->
    Fun = fun() -> mnesia:read({users, Login}) end,
    {atomic, Users} = mnesia:transaction(Fun),
    case Users of
	[] ->
	    {not_found};
	[U] ->
	    {ok, U}
    end.

handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(normal, _State) ->
    ok.
