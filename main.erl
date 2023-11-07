-module(main).
-export([start/0]).

start() ->
    shapes:process_file("commands.txt").