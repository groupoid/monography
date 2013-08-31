-module(pie).
-compile(export_all).

send(Pool, Message) -> gproc:send({p,l,Pool},Message).
reg(Pool) -> 
    Ctx = get({pool,Pool}),
    case Ctx of
         undefined -> gproc:reg({p,l,Pool}), put({pool,Pool},Pool);
         Defined -> skip end.

start(Args) ->
    application:start(sasl),
    application:start(compiler),
    application:start(syntax_tools),
    application:start(lager),
    application:start(gproc),
    application:start(sync),
    edit:start(Args).

start() ->
    start([]).