-module(shapes).
-export([add/2, del/2, output/1, process_file/1]).

-record(shape, {type, params, color, date}).
-record(point, {x, y}).
-record(base, {color, date}).
-record(circle, {center :: #point{}, radius :: number(), base :: #base{}}).
-record(triangle, {point1 :: #point{}, point2 :: #point{}, point3 :: #point{}, base :: #base{}}).
-record(rectangle, {point1 :: #point{}, point2 :: #point{}, base :: #base{}}).

% Функция добавления фигуры в контейнер
add(Container, Shape) ->
    Container ++ [Shape].

% Функция удаления элемента из контейнера
del(Container, Condition) ->
    lists:filter(fun(Shape) -> not match_shape(Shape, Condition) end, Container).

% Функция проверки соответствия фигуры условию
match_shape(#shape{type = Type, params = Params}, Condition) ->
    case Condition of
        {Type, _} -> true;
        {Type, Params} -> true;
        _ -> false
    end.

% Функция печати содержимого контейнера
output(Container) ->
    lists:foreach(fun(Shape) -> io:fwrite("~p~n", [Shape]) end, Container).

% Функция обработки файла с командами
process_file(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    process_lines(File),
    file:close(File).

% Функция обработки строк файла с командами
process_lines(File) ->
    Container = [],
    case io:get_line(File, "") of
        eof -> ok;
        Line -> process_line(Container, Line),
                process_lines(File)
    end.

% Функция обработки одной команды
process_line(Container, Line) ->
    [Command | Args] = string:tokens(Line, " "),
    case Command of
        "add" ->   process_add(Container, Args);
        "rem" ->   process_del(Container, Args);
        "print" -> process_print(Container);
        _ -> ok
    end.

% Функция обработки круга

% Функция обработки прямоугольника

% Функция обработки треугольника

% Функция обработки команды ADD
process_add(Container, Args) ->
    [Type | Params] = Args,
    lists:reverse(tl(lists:reverse(Args))),
    Shape = #shape{type = Type, params = Params, color = lists:last(Params), date = lists:last(lists:reverse(Params))},
    add(Container, Shape),
    io:fwrite("~p~n", [Shape]),
    io:fwrite("~p~n", [Type]).

% Функция обработки команды DEL
process_del(Container, Args) ->
    Condition = parse_condition(Args),
    io:fwrite("~p     ~n", [Condition]),
    del(Container, Condition).

% Функция обработки команды PRINT
process_print(Container) ->
    output(Container).

% Функция разбора условия удаления фигур
parse_condition(["*"]) ->
    all;
parse_condition([Type, Params]) ->
    {Type, Params};
parse_condition([Type]) ->
    {Type, undefined}.