-module(edit_input).
-author('maxim@synrc.com').
-include_lib("pie/include/edit.hrl").
-export([start_link/1, loop/1]).

%% Receiver will be sent {key_input, Char} each time a key is pressed.
start_link(Receiver) ->
    Pid = spawn_link(edit_input, loop, [Receiver]),
    register(?MODULE, Pid),
    Pid.

loop(Receiver) ->
    Ch = case ?EDIT_TERMINAL:read() of
        $\n -> $\r;
        145 -> panic(); % C-M-q is reserved for panic 
        219 -> case ?EDIT_TERMINAL:read() of
                    53 -> [219,53,?EDIT_TERMINAL:read()]; % Fn-UP
                    54 -> [219,54,?EDIT_TERMINAL:read()]; % Fn-DOWN
                    X -> Receiver ! {key_input, 219},
                         Receiver ! {key_input, X},
                         ?MODULE:loop(Receiver) end;
        59 -> case ?EDIT_TERMINAL:read() of
                    53 -> [59,53,?EDIT_TERMINAL:read()]; % CTRL+CURSON
                    50 -> [59,50,?EDIT_TERMINAL:read()]; % SHIFT+CURSOR
                    X -> Receiver ! {key_input, 59},
                         Receiver ! {key_input, X},
                         ?MODULE:loop(Receiver) end;
        207 -> case ?EDIT_TERMINAL:read() of
                    70 -> [207,70]; % Fn-RIGHT
                    72 -> [207,72]; % Fn-LEFT
                    X -> Receiver ! {key_input, 207},
                         Receiver ! {key_input, X},
                         ?MODULE:loop(Receiver) end;
        XXX -> XXX end,
    error_logger:info_msg("Input Char: ~p",[Ch]),
    Receiver ! {key_input, Ch},
    ?MODULE:loop(Receiver).

panic() -> halt().
