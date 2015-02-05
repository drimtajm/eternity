-module(eternityII).
-compile(export_all).

-include("../include/eternityII.hrl").
-define(DEFAULT_SIZE, {12, 6}).
-define(FILENAME(Size),
	io_lib:format("../resources/tiles~px~p.dat",
		      [element(2, Size), element(1, Size)])).

go() ->
    go(?DEFAULT_SIZE).

go(Size) ->
    Filename = ?FILENAME(Size),
    {ok, [Tiles]} = file:consult(Filename),
    CornerPieces = [eternityII_lib:make_tile(Tile) ||
		       {_, corner, _, _} = Tile <- Tiles],
    EdgePieces   = [eternityII_lib:make_tile(Tile) ||
		       {_, edge, _, _} = Tile <- Tiles],
    CenterPieces = [eternityII_lib:make_tile(Tile) ||
		       {_, center, _, _} = Tile <- Tiles],
    calculate_two_x_one_pieces(CenterPieces).

calculate_two_x_one_pieces(CenterPieces) ->
    calculate_two_x_one_pieces(CenterPieces, 0, []).

calculate_two_x_one_pieces([], 0, Result) ->
    Result;
calculate_two_x_one_pieces([], N, Result0) ->
    receive
	{true, Matches} ->
	    calculate_two_x_one_pieces([], N-1, Result0 ++ Matches);
	false ->
	    calculate_two_x_one_pieces([], N-1, Result0)
    end;
calculate_two_x_one_pieces([First | Rest] = Pieces, N, []) ->
    lists:map(fun (Tile) ->
		      spawn(eternityII, calculate_matches, [First, Tile, self()])
	      end, Pieces),
    calculate_two_x_one_pieces(Rest, N+length(Pieces), []).

calculate_matches(Tile1, Tile2, MasterPid) ->
    Result = eternityII_lib:matches(Tile1, Tile2),
    MasterPid ! Result.
