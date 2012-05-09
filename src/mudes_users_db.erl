-module(mudes_users_db).
-author('Alexander Svyazin <guybrush@live.ru>').

-behaviour(gen_server).

%% API
-export([add/2, exists/1, check_password/2]).

%% gen_server API
-export([start_link/2]).

%% callbacks
-export([init/1, handle_call/3]).

-record(state, {host, db}).

start_link(Host, Db) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Db], []).

init([Host, Db]) ->
    {ok, #state{host = Host, db = Db}}.

add(Login, Password) ->
    gen_server:call(?MODULE, {add, Login, Password}).

exists(Login) ->
    gen_server:call(?MODULE, {exists, Login}).

check_password(Login, Password) ->
    gen_server:call(?MODULE, {check_password, Login, Password}).

handle_call({add, Login, Password}, _From,
	    State = #state{host = Host, db = Db}) ->
    PasswordHash = crypto:md5(Password),
    {ok, Conn} = mongo:connect(Host),
    Doc = {login, Login, password_hash, {bin, md5, PasswordHash}},
    mongo:do(safe, master, Conn, Db, fun() -> mongo:insert(users, Doc) end),
    mongo:disconnect(Conn),
    {reply, ok, State};
handle_call({exists, Login}, _From,
	    State = #state{host = Host, db = Db}) ->
    {ok, Conn}= mongo:connect(Host),
    Result = exists_user(Conn, Db, Login),
    mongo:disconnect(Conn),
    {reply, Result, State};
handle_call({check_password, Login, Password}, _From,
	    State = #state{host = Host, db = Db}) ->
    {ok, Conn} = mongo:connect(Host),
    Result = do_check_password(Conn, Db, Login, Password),
    mongo:disconnect(Conn),
    {reply, Result, State}.

exists_user(Conn, Db, Login) ->
    case get_user(Conn, Db, Login) of
	{not_found} ->
	    false;
	{ok, _User} ->
	    true
    end.

get_user(Conn, Db, Login) ->
    Sel = {login, Login},
    {ok, QueryResult} = mongo:do(safe, slave_ok, Conn, Db,
				 fun() -> mongo:find_one(users, Sel) end),
    case QueryResult of
	{} ->
	    {not_found};
	{U} ->
	    {ok, U}
    end.

do_check_password(Conn, Db, Login, Password) ->
    PasswordHash = crypto:md5(Password),
    {ok, Doc} = get_user(Conn, Db, Login),
    {bin, md5, PasswordHashDb} = bson:at(password_hash, Doc),
    case PasswordHash of
	PasswordHashDb ->
	    true;
	_ ->
	    false
    end.
