-module(shapes).
-export([add/2, del/2, output/1, process_file/1]).

-record(point, {x, y}).
-record(base, {color, date}).
-record(circle, {center :: #point{}, radius :: number(), base :: #base{}}).
-record(rectangle, {point1 :: #point{}, point2 :: #point{}, base :: #base{}}).
-record(triangle, {point1 :: #point{}, point2 :: #point{}, point3 :: #point{}, base :: #base{}}).

% Функция добавления фигуры в контейнер
add(Container, Shape) ->
    lists:append(Container, [Shape]).

% Функция удаления элемента из контейнера
del(Container, Condition) ->
    lists:filter(fun(Shape) -> not Shape and Condition end, Container).
%    lists:filter(fun(Shape) -> not match_shape(Shape, Condition) end, Container).

% Функция проверки соответствия фигуры условию
%match_shape(#shape{type = Type, params = Params}, Condition) ->
%    case Condition of
%        {Type, _} -> true;
%        {Type, Params} -> true;
%        _ -> false
%    end.

% Функция печати содержимого контейнера
output(Container) ->
    lists:foreach(fun(Shape) -> io:fwrite("~p~n", [Shape]) end, Container).

% Функция обработки файла с командами
process_file(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    Container = [],
    NewContainer = process_lines(File, Container),
    io:format("Final array:\\n ~p~n", [NewContainer]),
    file:close(File).

% Функция обработки строк файла с командами
process_lines(File, Container) ->
    case io:get_line(File, "") of
        eof -> ok;
        Line -> 
            NewContainer = process_line(Container, Line),
            process_lines(File, NewContainer)
    end.

% Функция обработки одной команды
process_line(Container, Line) ->
    LineNew = [X || X<-Line, X =/= 10], % Удалить \n символ
    [Command | Args] = string:tokens(LineNew, " "),
    case Command of
        "add" ->   process_add(Container, Args);
        "rem" ->   process_del(Container, Args);
        "print" -> process_print(Container)
    end.

% Функция обработки круга
process_add_circle(Args) ->
    % Переменные
    XCoord = lists:nth(2, Args),
    YCoord = lists:nth(3, Args),
    RCoord = lists:nth(4, Args),
    Color  = lists:nth(5, Args),
    Date   = lists:nth(6, Args),
    
    % Инициализация
    #circle{
        center = #point{x = XCoord, y = YCoord},
        radius = RCoord,
        base = #base{color = Color, date = Date}
    }.
    % io:fwrite("~p~n", [Container]).
    

% Функция обработки прямоугольника
process_add_rectangle(Args) ->
    % Переменные
    X1Coord = lists:nth(2, Args),
    Y1Coord = lists:nth(3, Args),
    X2Coord = lists:nth(4, Args),
    Y2Coord = lists:nth(5, Args),
    Color  = lists:nth(6, Args),
    Date   = lists:nth(7, Args),
    
    % Инициализация
    #rectangle{
        point1 = #point{x = X1Coord, y = Y1Coord},
        point2 = #point{x = X2Coord, y = Y2Coord},
        base = #base{color = Color, date = Date}
    }.
    % io:fwrite("~p~n", [Container]).

% Функция обработки треугольника
process_add_triangle(Args) ->
    % Переменные
    X1Coord = lists:nth(2, Args),
    Y1Coord = lists:nth(3, Args),
    X2Coord = lists:nth(4, Args),
    Y2Coord = lists:nth(5, Args),
    X3Coord = lists:nth(6, Args),
    Y3Coord = lists:nth(7, Args),
    Color  = lists:nth(8, Args),
    Date   = lists:nth(9, Args),
    
    % Инициализация
    #triangle{
        point1 = #point{x = X1Coord, y = Y1Coord},
        point2 = #point{x = X2Coord, y = Y2Coord},
        point3 = #point{x = X3Coord, y = Y3Coord},
        base = #base{color = Color, date = Date}
    }.
    
    % io:fwrite("~p~n", [Container]).

% Функция обработки типа фигуры
figure_type_select(Type, Args) ->
    case Type of
        "circle" ->     process_add_circle(Args);
        "rectangle" ->  process_add_rectangle(Args);
        "triangle" ->   process_add_triangle(Args);
        _ -> error
    end.

% Функция обработки команды ADD
process_add(Container, Args) ->
    [Type | _] = Args,
    Shape = figure_type_select(Type, Args),
    % Shape = #shape{type = Type, params = Params, color = lists:last(Params), date = lists:last(lists:reverse(Params))},

    % io:fwrite("~p~n", [Container]),
    add(Container, Shape),
    io:fwrite("~p~n", [Container]).
    % io:fwrite("~p~n", [Args]).

% Функция обработки команды DEL
process_del(Container, Args) ->
    Condition = parse_condition(Args),
    % io:fwrite("~p     ~n", [Condition]),
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