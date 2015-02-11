-module(eternityII).
-compile(export_all).

-include("../include/eternityII.hrl").
-define(DEFAULT_SIZE, {12, 6}).
-define(FILENAME(Size),
        io_lib:format("../resources/tiles~px~p.dat",
                      [element(2, Size), element(1, Size)])).
-define(TABLE_NAME,  list_to_atom(lists:concat([?MODULE, "_1x2"]))).
-define(TABLE_NAME2, list_to_atom(lists:concat([?MODULE, "_2x2"]))).

go() ->
    go(?DEFAULT_SIZE).

go(Size) ->
    Filename = ?FILENAME(Size),
    {ok, [Tiles0]} = file:consult(Filename),
    Tiles = lists:map(fun eternityII_lib:make_tile/1, Tiles0),
    prepare_table(?TABLE_NAME),
    do_it(Tiles, ?TABLE_NAME),
    NewTiles = [T || {_Key, #tile{} = T} <- ets:tab2list(?TABLE_NAME)],
    cleanup_table(?TABLE_NAME, "tiles6x12_1x2.tab"),
    prepare_table(?TABLE_NAME2),
    do_it(NewTiles, ?TABLE_NAME2),
    cleanup_table(?TABLE_NAME2, "tiles6x12_2x2.tab").

prepare_table(TableName) ->
    ets:new(TableName, [ordered_set, named_table]),
    ets:insert(TableName, {current_id, 0}).
cleanup_table(TableName, FileName) ->
    ets:tab2file(TableName, FileName, [{extended_info, [md5sum]}]),
    true = ets:delete(TableName).

do_it(Tiles, TableName) ->
    CornerPieces = [Corner || #tile{type=corner} = Corner <- Tiles],
    EdgePieces   = [Edge   || #tile{type=edge}   = Edge   <- Tiles],
    CenterPieces = [Center || #tile{type=center} = Center <- Tiles],
    {Time, ok} = timer:tc(?MODULE, match_it,
                          [CornerPieces, EdgePieces, CenterPieces, TableName]),
    io:format("Time: ~p. Got ~p matches.~n",
              [Time, ets:info(TableName, size)-1]).

match_it(CornerPieces, EdgePieces, CenterPieces, TableName) ->
    match_pieces(CornerPieces, EdgePieces, TableName),
    match_pieces(EdgePieces, TableName),
    match_pieces(EdgePieces, CenterPieces, TableName),
    match_pieces(CenterPieces, TableName),
    ok.

match_pieces(Pieces, TableName) ->
    generate_pieces(Pieces, 0, TableName).
match_pieces(PiecesTypeA, PiecesTypeB, TableName) ->
    generate_pieces(PiecesTypeA, PiecesTypeB, 0, TableName).

generate_pieces([], 0, _) ->
    ok;
generate_pieces([], N, TableName) ->
    receive
        {true, Matches} ->
            add_pieces_to_db(Matches, TableName),
            generate_pieces([], N-1, TableName)
    end;
generate_pieces([First | Rest] = Pieces, N, TableName) ->
    if First#tile.count < 2 ->
            %% Don't match tile with itself, because it is unique
            spawn(eternityII, process_dataset, [First, Rest, self()]);
       true ->
            spawn(eternityII, process_dataset, [First, Pieces, self()])
    end,
    generate_pieces(Rest, N+1, TableName).

generate_pieces([], [], 0, _) ->
    ok;
generate_pieces([], _PiecesTypeB, N, TableName) ->
    receive
        {true, Matches} ->
            add_pieces_to_db(Matches, TableName),
            generate_pieces([], [], N-1, TableName)
    end;
generate_pieces([First | Rest], PiecesTypeB, N, TableName) ->
    spawn(eternityII, process_dataset, [First, PiecesTypeB, self()]),
    generate_pieces(Rest, PiecesTypeB, N+1, TableName).

process_dataset(Piece, List, Master) ->
    Result0 = lists:map(fun (Tile) ->
                                eternityII_lib:matches(Piece, Tile)
                        end, List),
    Result = [Matches || {true, Matches} <- Result0],
    Master ! {true, lists:flatten(Result)}.

add_pieces_to_db([], _) ->
    ok;
add_pieces_to_db([#tile{type=Type, pattern=Pattern0,
                        sources=[Source0]} = Piece0 | Rest], TableName) ->
    Pattern = eternityII_lib:sort_pattern(Pattern0),
    Key = {Type, Pattern},
    case ets:lookup(TableName, Key) of
        [] ->
            NextId = ets:update_counter(TableName, current_id, 1),
            Size = case get_size(Pattern) of
                       old_size -> Piece0#tile.size;
                       Size0    -> Size0
                   end,
            Piece = Piece0#tile{id = NextId, pattern=Pattern, size=Size},
            ets:insert(TableName, {Key, Piece});
        [{Key, #tile{count=Count, sources=OldSources}=Tile0}] ->
            case lists:member(Source0, OldSources) of
               true  -> nothing_to_do;
               false ->
                    Tile = Tile0#tile{count=Count+1,
                                      sources=[Source0 | OldSources]},
                    ets:update_element(TableName, Key, {2, Tile})
            end
    end,
    add_pieces_to_db(Rest, TableName).

get_size(#pattern{right=Right, down=Down}) when Right /= undefined
                                                andalso Down /= undefined ->
    {length(Down), length(Right)};
get_size(_) -> old_size.

