-module(eternityII).
-compile(export_all).

-include("../include/eternityII.hrl").
-define(DEFAULT_SIZE, {12, 6}).
-define(FILENAME(Size),
        lists:concat(["../resources/tiles", element(2, Size),
                      x, element(1, Size), ".dat"])).
-define(FILENAME_STRATEGY(Size),
        lists:concat(["../resources/tiles", element(2, Size),
                      x, element(1, Size), ".strategy"])).
-define(TABLE_NAME(X,Y),
        list_to_atom(lists:concat([?MODULE, "_", X, x, Y]))).
-define(TABLE_FILENAME(X,Y,Size),
        lists:concat(["../resources/tiles", element(2, Size), x,
                      element(1, Size), "_", X, x, Y, ".tab"])).

go() ->
    go(?DEFAULT_SIZE).

go(Size) ->
    FilenameStrategy = ?FILENAME_STRATEGY(Size),
    {ok, Strategy} = file:consult(FilenameStrategy),
    loop(Strategy, Size).

loop([], _) ->
    done;
loop([{{X1, Y1}, {X2, Y2}, {X, Y, ResultingSize}} | Rest], Size) ->
%%    {Tiles1, Tiles2} =
%%        if ((X1 == X2) andalso (Y1 == Y2)) ->
%%                Tiles = get_tiles(X1, Y1, Size),
%%                {Tiles, Tiles};
%%           true  ->
%%                {get_tiles(X1, Y1, Size),
%%                 get_tiles(X2, Y2, Size)}
%%        end,
    eternityIIdb:start_link({X1, Y1}, {X2, Y2}, {X, Y}),
    do_it(ResultingSize),
    %%do_it(Tiles1, Tiles2, ?TABLE_NAME(X,Y), ResultingSize),
    eternityIIdb:stop(),
%%    cleanup_table(?TABLE_NAME(X,Y), ?TABLE_FILENAME(X, Y, Size)),
    loop(Rest, Size).

%%================================
%% REMOVE
get_first_tiles(Size) ->
    Filename = ?FILENAME(Size),
%%    io:format("Reading from file: ~p~n", [Filename]),
    {ok, [Tiles0]} = file:consult(Filename),
    lists:map(fun eternityII_lib:make_tile/1, Tiles0).

get_tiles(1, 1, Size) ->
    get_first_tiles(Size);
get_tiles(X, Y, Size) ->
    Filename = ?TABLE_FILENAME(X, Y, Size),
%%    io:format("Reading from file: ~p~n", [Filename]),
    {ok, Tab} = ets:file2tab(Filename),
    Tiles = [T || {_Key, #tile{} = T} <- ets:tab2list(Tab)],
    ets:delete(Tab),
    Tiles.


do_it(Tiles1, Tiles2, TableName, ResultingSize) ->
    CornerPieces1 = [Corner || #tile{type=corner} = Corner <- Tiles1],
    EdgePieces1   =
        chunkify([Edge   || #tile{type=edge}   = Edge   <- Tiles1]),
    CenterPieces1 =
        chunkify([Center || #tile{type=center} = Center <- Tiles1]),
    CornerPieces2 = [Corner || #tile{type=corner} = Corner <- Tiles2],
    EdgePieces2   =
        chunkify([Edge   || #tile{type=edge}   = Edge   <- Tiles2]),
    CenterPieces2 =
        chunkify([Center || #tile{type=center} = Center <- Tiles2]),
    {Time, ok} = timer:tc(?MODULE, match_it,
                          [CornerPieces1, EdgePieces1, CenterPieces1,
                           CornerPieces2, EdgePieces2, CenterPieces2,
                           ResultingSize, TableName]),
    io:format("Time: ~s. Got ~p matches.~n",
              [format_time(Time), ets:info(TableName, size)-1]).
%%================================

do_it(ResultingSize) ->
    {Time, ok} = timer:tc(?MODULE, match_it, [ResultingSize]),
    io:format("Time: ~s.~n", [format_time(Time)]).

match_it(ResultingSize) ->
    %% TODO: recurse and handle return value no_data_left
    {ok, Pattern, CornerPieces2} = eternityIIdb:get_chunk(corner),
    {ok, EdgePieces1} = eternityIIdb:get_chunk(edge, Pattern),
    generate_pieces2(CornerPieces2, EdgePieces1, ResultingSize),
    garbage_collect(),
    ok.

match_it(_CornerPieces1, EdgePieces1, CenterPieces1,
         CornerPieces2, EdgePieces2, CenterPieces2,
         ResultingSize, TableName) ->
    lists:foreach(fun (EdgePieces1Chunk) ->
                          match_pieces(CornerPieces2, EdgePieces1Chunk,
                                       TableName, ResultingSize)
                  end, EdgePieces1),
    io:format("corners done~n"),
    case ResultingSize of
        square ->
            lists:foreach(
              fun (EdgePieces1Chunk) ->
                      lists:foreach(fun (EdgePieces2Chunk) ->
                                            match_pieces(EdgePieces2Chunk,
                                                         EdgePieces1Chunk,
                                                         TableName,
                                                         ResultingSize)
                                    end, EdgePieces2)
              end, EdgePieces1);
        non_square ->
            lists:foreach(
              fun (EdgePieces2Chunk) ->
                      lists:foreach(fun (CenterPieces1Chunk) ->
                                            match_pieces(EdgePieces2Chunk,
                                                         CenterPieces1Chunk,
                                                         TableName,
                                                         ResultingSize)
                                    end, CenterPieces1)
              end, EdgePieces2)
    end,
    io:format("edges done~n"),
    lists:foreach(
      fun (CenterPieces1Chunk) ->
              lists:foreach(fun (CenterPieces2Chunk) ->
                                    match_pieces(CenterPieces2Chunk,
                                                 CenterPieces1Chunk,
                                                 TableName, ResultingSize)
                            end, CenterPieces2)
      end, CenterPieces1),
    io:format("centerpieces done~n"),
    ok.

%%match_pieces(Pieces, TableName, ResultingSize) ->
%%    generate_pieces(Pieces, 0, TableName, ResultingSize).
match_pieces(PiecesTypeA, PiecesTypeB, TableName, ResultingSize) ->
    generate_pieces(PiecesTypeA, PiecesTypeB, 0, TableName, ResultingSize),
    garbage_collect().

generate_pieces([], 0, _, _ResultingSize) ->
    ok;
generate_pieces([], N, TableName, ResultingSize) ->
    receive
        {true, Matches} ->
            add_pieces_to_db(Matches, TableName),
            generate_pieces([], N-1, TableName, ResultingSize)
    end;
generate_pieces([First | Rest] = Pieces, N, TableName, ResultingSize) ->
    if First#tile.count < 2 ->
            %% Don't match tile with itself, because it is unique
            spawn(eternityII, process_dataset,
                  [First, Rest, ResultingSize, self()]);
       true ->
            spawn(eternityII, process_dataset,
                  [First, Pieces, ResultingSize, self()])
    end,
    generate_pieces(Rest, N+1, TableName, ResultingSize).

generate_pieces([], [], 0, _, _ResultingSize) ->
    ok;
generate_pieces([], _PiecesTypeB, N, TableName, ResultingSize) ->
    receive
        {true, Matches} ->
            add_pieces_to_db(Matches, TableName),
            generate_pieces([], [], N-1, TableName, ResultingSize)
    end;
generate_pieces([First | Rest] = PiecesA, PiecesB,
                N, TableName, ResultingSize) ->
    spawn(eternityII, process_dataset,
          [First, PiecesB, ResultingSize, self()]),
    NewPiecesB = case PiecesB of
                     PiecesA -> Rest;
                     _       -> PiecesB
                 end,
    generate_pieces(Rest, NewPiecesB, N+1, TableName, ResultingSize).


generate_pieces2([], [], 0, _ResultingSize) ->
    ok;
generate_pieces2([], _PiecesTypeB, N, ResultingSize) ->
    receive
        {true, Matches} ->
%%            add_pieces_to_db(Matches, TableName),
            eternityIIdb:add_tiles_to_db(Matches),
            generate_pieces2([], [], N-1, ResultingSize)
    end;
generate_pieces2([First | Rest] = PiecesA, PiecesB,
                 N, esultingSize) ->
    spawn(eternityII, process_dataset,
          [First, PiecesB, ResultingSize, self()]),
    NewPiecesB = case PiecesB of
                     PiecesA -> Rest;
                     _       -> PiecesB
                 end,
    generate_pieces2(Rest, NewPiecesB, N+1, ResultingSize).

process_dataset(Piece, List, ResultingSize, Master) ->
    Result0 = lists:map(fun (Tile) ->
                                eternityII_lib:match(Piece, Tile,
                                                     ResultingSize)
                        end, List),
    Result = [Matches || {true, Matches} <- Result0],
    Master ! {true, lists:flatten(Result)}.

save_under_pattern(#pattern{left=P1, up=P2, right=P3, down=P4},
                   Id, Tablename) ->
    save_pattern_index(P1, Id, Tablename),
    save_pattern_index(P2, Id, Tablename),
    save_pattern_index(P3, Id, Tablename),
    save_pattern_index(P4, Id, Tablename).

save_pattern_index(undefined, _, _) ->
    noop;
save_pattern_index(Pattern, Id, Tablename) ->
    case ets:lookup(Tablename, Pattern) of
        [] ->
            ets:insert(Tablename, {Pattern, [Id]}),
            [{patterns, Patterns}] = ets:lookup(Tablename, patterns),
            ets:update_element(Tablename, patterns, {2, [Pattern | Patterns]});
        [{Pattern, Ids}] ->
            ets:update_element(Tablename, Pattern, {2, [Id | Ids]})
    end.

chunkify(List) ->
    chunkify(List, 100).
chunkify(List, Max) ->
    chunkify(List, Max, 0, [], []).

chunkify([], _, _, Result, []) ->
    lists:reverse(Result);
chunkify([], _, _, Result, Acc) ->
    lists:reverse([lists:reverse(Acc) | Result]);
chunkify(List, Max, Max, Result, Acc) ->
    chunkify(List, Max, 0, [lists:reverse(Acc) | Result], []);
chunkify([First | Rest], Max, N, Result, Acc) ->
    chunkify(Rest, Max, N+1, Result, [First | Acc]).


format_time(Time) when Time < 1000000 ->
    io_lib:format("~p ms", [Time div 1000]);
format_time(Time) ->
    io_lib:format("~.2f s", [Time / 1000000]).
