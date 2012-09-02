-module(mudes_users_db_tests).
-include_lib("eunit/include/eunit.hrl").

-define(LOGIN, <<"test_login">>).
-define(PASSWORD, <<"test_password">>).

all_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     ?_test(
	begin
	    ?assertNot(mudes_users_db:exists(?LOGIN)),
	    ?assertMatch(ok, mudes_users_db:add(?LOGIN, ?PASSWORD)),
	    ?assert(mudes_users_db:exists(?LOGIN)),
	    ?assert(mudes_users_db:check_password(?LOGIN, ?PASSWORD)),
	    ?assertMatch(ok, mudes_users_db:remove(?LOGIN)),
	    ?assertNot(mudes_users_db:exists(?LOGIN))
	end)}.

setup() ->
    mnesia:create_schema([node()]),
    ok = mnesia:start(),
    mudes_users_db:create_tables(),
    mudes_users_db:start_link().

cleanup(_Pid) ->
    mudes_users_db:stop(),
    mnesia:stop().
