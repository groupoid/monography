-module(pie_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-compile(export_all).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    edit:start(),

    {ok, {{one_for_one, 5, 10}, []}}.

