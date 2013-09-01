Programmer's Interactive Editor
===============================

Emacs-like Erlang editor fully written in Erlang with S-Lang driver.
It is refined and ready to run on R16B01 with built-in sync and lager.
Pie codebase is very tiny, clean and extendable.

![Pie](http://synrc.com/lj/pie2.png)

Run
---

    $ rebar get-deps
    $ rebar compile
    $ ./pie README.md

Emacs CUA mode
--------------

Pie suports Extended Kystrokes and CUA mode being general purpose
editor saving your hands from RSI.

    {"Fn-HOME", {edit_lib, beginning_of_line, []}},
    {"Fn-END",  {edit_lib, end_of_line, []}},
    {"C-LEFT",  {edit_lib, backward_word, []}},
    {"C-RIGHT", {edit_lib, forward_word, []}},
    {"Fn-PGDN", {edit_lib, scroll_down, []}},
    {"Fn-PGUP", {edit_lib, scroll_up, []}},
    {"DEL",     {edit_lib, delete_char_forward, []}},

Remember that Pie is an Emacs (Erlang Macros) editor thus
it supports all basic Emacs commands which you can find
in edit_globalmap bindings. Here are most important:

File Operations
---------------

    {"C-c", {edit_lib, quit, []}},
    {"C-f", {edit_file, find_file, []}},
    {"C-s", {edit_file, save_file, []}},

Window Management "C-x" extensions
----------------------------------

    {"o",   {edit_lib, next_window, []}},
    {"0",   {edit_lib, delete_window, []}},
    {"1",   {edit_lib, delete_other_windows, []}},
    {"2",   {edit_lib, split_window_vertically, []}},

Pie Commands
------------

"M-x" will run internal Pie command in module:function format.
E.g. you can type "edit_lib:scroll_up"

For developing Pie commands a parse_transform syntax was introduced.
Here is example of edit_lib:unix_command

    -command({unix_command, [{shell_command, "Shell command:"}]}).
    unix_command(State, Cmd) ->
        Text = os:cmd(["sh -c '(cd ",edit_util:pwd(State)," ; ",Cmd,")'"]),
        edit_util:popup_message(State, '*Shell Command Output*', Text).

State is mandatory first argument of Pie state.

Eval
----

"M-:" command will evaluate Erlang Expression in the minibuffer,
like "os:type()." or "lists:seq(1,100).". If output is not fit into
one line another window will be created.

Interactive Mode
----------------

"C-x i" command will enter Erlang Interactive shell where you can
evaluate Erlang expressions inside editor buffer by pressing ENTER on them.
The result would be displayed also in the editor buffer below.

    os:type(). ENTER
    => {unix,linux}
    >>

Fundamental Mode
----------------

"C-x f" command will bring you back to Fundamental mode which
is defaul editing mode.

Erlang Mode
-----------

"C-x e" command will dive you into Erlang mode which helps you edit Erlang code.

Futher tasks
------------

* Syntax Coloring
* Selection

Credits
-------

* Luke Gorrie
* Maxim Sokhatsky

Notes
-----

This file was edited using Pie.
Pie supports self-developing using sync.

OM A HUM
