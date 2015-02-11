%%%-------------------------------------------------------------------
%%% @author  <firefly@scorpion>
%%% @doc
%%%
%%% @end
%%% Created : 30 Jan 2015 by  <firefly@scorpion>
%%%-------------------------------------------------------------------
-module(eternityII_lib).

-include("../include/eternityII.hrl").

%% API
%%-export([]).
-compile(export_all).

%%%===================================================================
%%% API
%%%===================================================================

make_tile({Id,Type,Pattern,Sources}) ->
    #tile{id = Id, type = Type, pattern = make_pattern(Pattern),
          size = {1,1}, count = length(Sources),
          sources = Sources, primary = []}.

make_pattern(Pattern) ->
    sort_pattern(make_pattern_aux(Pattern)).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% case 0: matching a corner_slice with another (end condition)
%%--------------------------------------------------------------------
matches(#tile{id=Id1, type=corner_slice, size={X, Y},
              pattern=#pattern{left = undefined, up = undefined,
                               down = undefined, right = Pattern1}},
        #tile{id=Id2, type=corner_slice, size={X, Y},
              pattern=#pattern{left = undefined, up = undefined,
                               down = undefined, right = Pattern2}}) ->
    case patterns_match(Pattern1, Pattern2) of
        false -> false;
        true  -> Solution = #tile{type=solution, size={2*X, Y},
                                  count=1, sources=[{Id1, Id2}]},
                 {true, [Solution]}
    end;

%%--------------------------------------------------------------------
%% case 1a: matching a corner_slice with a slice,
%%          making a bigger corner_slice
%%--------------------------------------------------------------------
%%matches(#tile{id=Id1, type=corner_slice, size={X1, Y},
%%              pattern=#pattern{left = undefined, up = undefined,
%%                               down = undefined, right = Right1}},
%%        #tile{id=Id2, type=slice, size={X2, Y},
%%              pattern=#pattern{left = Left2, up = undefined,
%%                               down = undefined, right = Right2}}) ->
%%    case patterns_match(Right1, Left2) of
%%        false -> case patterns_match(Right1, Right2) of
%%                     false -> false;
%%                     true  ->
%%                         {true, [#tile{type=corner_slice, size={X1+X2, Y},
%%                                       pattern=#pattern{right=Left2},
%%                                       count=1, sources=[{Id1, Id2}]}]}
%%                 end;
%%        true  ->
%%            {true, [#tile{type=corner_slice, size={X1+X2, Y},
%%                          pattern=#pattern{right=Right2},
%%                          count=1, sources=[{Id1, Id2}]}]}
%%    end;

%%--------------------------------------------------------------------
%% case 1b: matching a slice with another, making a bigger slice
%%--------------------------------------------------------------------
%%matches(#tile{id=Id1, type=slice, size={X1, Y},
%%              pattern=#pattern{left = Left1, up = undefined,
%%                               down = undefined, right = Right1}},
%%        #tile{id=Id2, type=slice, size={X2, Y},
%%              pattern=#pattern{left = Left2, up = undefined,
%%                               down = undefined, right = Right2}}) ->
%%    Match0 = case patterns_match(Right1, Left2) of
%%                 false -> case patterns_match(Right1, Right2) of
%%                     false -> false;
%%                     true  ->
%%                         #tile{type=slice, size={X1+X2, Y},
%%                               pattern=#pattern{left=Left1, right=Left2},
%%                               count=1, sources=[{Id1, Id2}]}
%%                 end;
%%                 true  ->
%%                     #tile{type=slice, size={X1+X2, Y},
%%                           pattern=#pattern{left=Left1, right=Right2},
%%                           count=1, sources=[{Id1, Id2}]}
%%             end,
%%    case patterns_match(Left1, Right1) of
%%        true  -> Match0;
%%        false ->
%%            Match1 =
%%                case patterns_match(Left1, Left2) of
%%                    false ->
%%                        case patterns_match(Left1, Right2) of
%%                            false -> false;
%%                            true  ->
%%                                #tile{type=slice, size={X1+X2, Y},
%%                                      pattern=#pattern{left=Right1,
%%                                                       right=Left2},
%%                                      count=1, sources=[{Id1, Id2}]}
%%                        end;
%%                    true  ->
%%                        #tile{type=slice, size={X1+X2, Y},
%%                              pattern=#pattern{left=Right1, right=Right2},
%%                              count=1, sources=[{Id1, Id2}]}
%%                end,
%%            case {Match0, Match1} of
%%                {false, false} -> false;
%%                {false, _}     -> {true, [Match1]};
%%                {_, false}     -> {true, [Match0]};
%%                _Else          -> {true, [Match0, Match1]}
%%            end
%%    end;

%%--------------------------------------------------------------------
%% case 2a: matching a corner with a corner, making a corner_slice
%%--------------------------------------------------------------------
%%matches(#tile{id=Id1, type=corner, size={X1, Y1},
%%              pattern=#pattern{left = undefined, up = undefined,
%%                               right = Right1, down = Down1}},
%%        #tile{id=Id2, type=corner, size={X2, Y2},
%%              pattern=#pattern{left = undefined, up = undefined,
%%                               right = Right2, down = Down2}}) ->
%%    Match0 = case patterns_match(Right1, Down2) of
%%                 false -> false;
%%                 true  ->
%%                     #tile{type=corner_slice, size={Y1, X1+Y2},
%%                           pattern=#pattern{right = Right2++Down1},
%%                           count=1, sources=[{Id1, Id2}]}
%%             end,
%%    Match1 = case patterns_match(Down1, Right2) of
%%                 false -> false;
%%                 true ->
%%                     #tile{type=corner_slice, size={X1, Y1+X2},
%%                           pattern=#pattern{right = Right1++Down2},
%%                           count=1, sources=[{Id1, Id2}]}
%%             end,
%%    case {Match0, Match1} of
%%        {false, false} -> false;
%%        {false, _}     -> {true, [Match1]};
%%        {_, false}     -> {true, [Match0]};
%%        _Else          -> {true, [Match0, Match1]}
%%    end;

%%--------------------------------------------------------------------
%% case 2b: matching an edge with an edge, making a slice
%%--------------------------------------------------------------------
%%matches(#tile{id=Id1, type=edge, size={X,Y1},
%%              pattern=#pattern{left = Left1, up = undefined,
%%                               right = Right1, down = Down1}},
%%        #tile{id=Id2, type=edge, size={X,Y2},
%%              pattern=#pattern{left = Left2, up = undefined,
%%                               right = Right2, down = Down2}}) ->
%%    case patterns_match(Down1, Down2) of
%%        false -> false;
%%        true  -> {true, [#tile{type=slice, size={X, Y1+Y2},
%%                               pattern=#pattern{left = Right2++Left1,
%%                                                right = Right1++Left2},
%%                               count=1, sources=[{Id1, Id2}]}]}
%%    end;

%%--------------------------------------------------------------------
%% case 3a: matching a corner with an edge, making a bigger corner
%%--------------------------------------------------------------------
matches(#tile{id=IdL, type=corner, size={XL, YL},
              pattern=#pattern{left = undefined, up = undefined,
                               right = RightL, down = DownL}},
        #tile{id=IdR, type=edge, size={XR, YR},
              pattern=#pattern{left = LeftR, up = undefined,
                               right = RightR, down = DownR}})
  when YR == YL orelse YR == XL ->
    Match0 = case patterns_match(RightL, LeftR) of
                 false -> false;
                 true  ->
                     #tile{type=corner, size={XL + XR, YL},
                           pattern=#pattern{right = RightR,
                                            down = DownR++DownL},
                           count=1, sources=[{IdL, IdR}]}
             end,
    Match1 = case patterns_match(DownL, RightR) of
                 false -> false;
                 true  ->
                     #tile{type=corner, size={XL, YL + XR},
                           pattern=#pattern{right = RightL++DownR,
                                            down=LeftR},
                           count=1, sources=[{IdL, IdR}]}
             end,
    case {Match0, Match1} of
        {false, false} -> false;
        {false, _}     -> {true, [Match1]};
        {_, false}     -> {true, [Match0]};
        _Else          -> {true, [Match0, Match1]}
    end;
matches(#tile{type=corner} = _Corner, #tile{type=edge} = _Edge) ->
    false;

%%--------------------------------------------------------------------
%% case 3b: matching an edge with an edge, making a bigger edge
%%--------------------------------------------------------------------
matches(#tile{id=IdL, type=edge, size={XL, Y},
              pattern=#pattern{left = LeftL, up = undefined,
                               right = RightL, down = DownL}},
        #tile{id=IdR, type=edge, size={XR, Y},
              pattern=#pattern{left = LeftR, up = undefined,
                               right = RightR, down = DownR}}) ->
    Match0 = case patterns_match(RightL, LeftR) of
                 false -> false;
                 true  ->
                     #tile{type=edge, size={XL+XR, Y},
                           pattern=#pattern{left = LeftL, right = RightR,
                                            down = DownR++DownL},
                           count=1, sources=[{IdL, IdR}]}
             end,
    Match1 = case patterns_match(RightR, LeftL) of
                 false -> false;
                 true  ->
                     #tile{type=edge, size={XL+XR, Y},
                           pattern=#pattern{left = LeftR, right = RightL,
                                            down = DownL++DownR},
                           count=1, sources=[{IdR, IdL}]}
             end,
    case {Match0, Match1} of
        {false, false} -> false;
        {false, _}     -> {true, [Match1]};
        {_, false}     -> {true, [Match0]};
        _Else          -> {true, [Match0, Match1]}
    end;
matches(#tile{type=edge} = _EdgeL, #tile{type=edge}  = _EdgeR) ->
    false;

%%--------------------------------------------------------------------
%% case 3c: matching an edge with a centerpiece, making a bigger edge
%%--------------------------------------------------------------------
matches(#tile{id=IdL, type=edge, size={XL, YL},
              pattern=#pattern{left = LeftL, up = undefined,
                               right = RightL, down = DownL}},
        #tile{id=IdR, type=center, size={XR, YR},
              pattern=#pattern{left = LeftR, up = UpR,
                               right = RightR, down = DownR}}) ->
    Match0 = case patterns_match(DownL, UpR) of
                 false -> false;
                 true  ->
                     #tile{type=edge, size={XL, YL+YR},
                           pattern=#pattern{left = LeftR++LeftL, down = DownR,
                                            right = RightL++RightR},
                           count=1, sources=[{IdL, IdR}]}
             end,
    Match1 = case patterns_match(DownL, DownR) of
                 false -> false;
                 true  ->
                     #tile{type=edge, size={XL, YL+YR},
                           pattern=#pattern{left = RightR++LeftL, down = UpR,
                                            right = RightL++LeftR},
                           count=1, sources=[{IdL, IdR}]}
             end,
    Match2 = case patterns_match(DownL, RightR) of
                 false -> false;
                 true  ->
                     #tile{type=edge, size={XL, YL+XR},
                           pattern=#pattern{left = UpR++LeftL, down = LeftR,
                                            right = RightL++DownR},
                           count=1, sources=[{IdL, IdR}]}
             end,
    Match3 = case patterns_match(DownL, LeftR) of
                 false -> false;
                 true  ->
                     #tile{type=edge, size={XL, YL+XR},
                           pattern=#pattern{left = DownR++LeftL, down = RightR,
                                            right = RightL++UpR},
                           count=1, sources=[{IdL, IdR}]}
             end,
    Matches = [M || #tile{}=M <- [Match0, Match1, Match2, Match3]],
    case Matches of
        [] -> false;
        _  -> {true, Matches}
    end;

%%--------------------------------------------------------------------
%% case 4: matching two centerpieces, making a bigger centerpiece
%%--------------------------------------------------------------------
%% trivial case: the two centerpieces are the same
matches(#tile{id=Id, type=center, size = {X, Y},
              pattern=#pattern{left=Left, up=Up,
                               right=Right, down=Down}} = Tile, Tile) ->
    Matches0 = [#tile{type=center, size = {Y, 2*X},
                      pattern=#pattern{up = Left, right = Up++Down,
                                       down = Left, left = Up++Down},
                      count = 1, sources=[{Id, Id}]},
                #tile{type=center, size = {Y, 2*X},
                      pattern=#pattern{up = Right, right = Down++Up,
                                       down = Right, left = Down++Up},
                      count = 1, sources=[{Id, Id}]}],
    Matches1 = if (Y == X) ->
                       [#tile{type=center, size = {Y, 2*X},
                              pattern=#pattern{up = Down, right = Left++Right,
                                               down = Down, left = Left++Right},
                              count = 1, sources=[{Id, Id}]},
                        #tile{type=center, size = {Y, 2*X},
                              pattern=#pattern{up = Up, right = Right++Left,
                                               down = Up, left = Right++Left},
                              count = 1, sources=[{Id, Id}]}
                        | Matches0];
                  true ->
                       Matches0
               end,
    {true, Matches1};

%%--------------------------------------------------------------------
%% case B: different tiles
%%--------------------------------------------------------------------
matches(#tile{type=center} = TileL, #tile{type=center} = TileR) ->
    MatchesLeft  = match_left(TileL, TileR),
    MatchesUp    = match_up(TileL, TileR),
    MatchesRight = match_right(TileL, TileR),
    MatchesDown  = match_down(TileL, TileR),
    Matches = MatchesLeft ++ MatchesUp ++ MatchesRight ++ MatchesDown,
    case Matches of
        [] -> false;
        _  -> {true, Matches}
    end.

match_left(TileL, TileR) ->
    M0 = match_left_right(TileL, TileR),
    M1 = match_left_left(TileL, TileR),
    M2 = match_left_up(TileL, TileR),
    M3 = match_left_down(TileL, TileR),
    [M || M <- [M0, M1, M2, M3], M /= false].

match_right(TileL, TileR) ->
    M0 = match_right_right(TileL, TileR),
    M1 = match_right_left(TileL, TileR),
    M2 = match_right_up(TileL, TileR),
    M3 = match_right_down(TileL, TileR),
    [M || M <- [M0, M1, M2, M3], M /= false].

match_up(TileL, TileR) ->
    M0 = match_up_right(TileL, TileR),
    M1 = match_up_left(TileL, TileR),
    M2 = match_up_up(TileL, TileR),
    M3 = match_up_down(TileL, TileR),
    [M || M <- [M0, M1, M2, M3], M /= false].

match_down(TileL, TileR) ->
    M0 = match_down_right(TileL, TileR),
    M1 = match_down_left(TileL, TileR),
    M2 = match_down_up(TileL, TileR),
    M3 = match_down_down(TileL, TileR),
    [M || M <- [M0, M1, M2, M3], M /= false].

%% match left pattern on left tile
match_left_left(#tile{id=IdL, type=center, size={XL, Y},
                      pattern=#pattern{left=LeftL, up=UpL,
                                       right=RightL, down=DownL}},
                #tile{id=IdR, type=center, size={XR, Y},
                      pattern=#pattern{left=LeftR, up=UpR,
                                       right=RightR, down=DownR}}) ->
    case patterns_match(LeftL, LeftR) of
        false -> false;
        true  -> #tile{type=center, size={Y, XL+XR},
                       pattern=#pattern{up = RightL, right = DownL++UpR,
                                        down = RightR, left = DownR++UpL},
                       count=1, sources=[{IdL, IdR}]}
    end;
match_left_left(_, _) -> false.

match_left_right(#tile{id=IdL, type=center, size={XL, Y},
                       pattern=#pattern{left = LeftL, up = UpL,
                                        right = RightL, down = DownL}},
                 #tile{id=IdR, type=center, size={XR, Y},
                       pattern=#pattern{left = LeftR, up = UpR,
                                        right = RightR, down = DownR}}) ->
    case patterns_match(LeftL, RightR) of
        false -> false;
        true  -> #tile{type=center, size={Y, XL+XR},
                       pattern=#pattern{up = RightL, right = DownL++DownR,
                                        down = LeftR, left = UpR++UpL},
                       count=1, sources=[{IdL, IdR}]}
    end;
match_left_right(_, _) -> false.


match_left_up(#tile{id=IdL, type=center, size={X, YL},
                    pattern=#pattern{left = LeftL, up = UpL,
                                     right = RightL, down = DownL}},
              #tile{id=IdR, type=center, size = {X, YR},
                    pattern=#pattern{left = LeftR, up = UpR,
                                     right = RightR, down = DownR}}) ->
    case patterns_match(LeftL, UpR) of
        false -> false;
        true  -> #tile{type=center, size={YL, X+YR},
                       pattern=#pattern{up = RightL, right = DownL++RightR,
                                        down = DownR, left = LeftR++UpL},
                       count=1, sources=[{IdL, IdR}]}
    end;
match_left_up(_, _) -> false.


match_left_down(#tile{id=IdL, type=center, size = {X, YL},
                      pattern=#pattern{left = LeftL, up = UpL,
                                       right = RightL, down = DownL}},
                #tile{id=IdR, type=center, size = {X, YR},
                      pattern=#pattern{left = LeftR, up = UpR,
                                       right = RightR, down = DownR}}) ->
    case patterns_match(LeftL, DownR) of
        false -> false;
        true  -> #tile{type=center, size={YL, X+YR},
                       pattern=#pattern{up = RightL, right = DownL++LeftR,
                                        down = UpR, left = RightR++UpL},
                       count=1, sources=[{IdL, IdR}]}
    end;
match_left_down(_, _) -> false.


%% match right pattern on left tile
match_right_left(#tile{id=IdL, type=center, size={XL, Y},
                       pattern=#pattern{left = LeftL, up = UpL,
                                        right = RightL, down = DownL}},
                 #tile{id=IdR, type=center, size={XR, Y},
                       pattern=#pattern{left = LeftR, up = UpR,
                                        right = RightR, down = DownR}}) ->
    case patterns_match(RightL, LeftR) of
        false -> false;
        true  -> #tile{type=center, size={Y, XL+XR},
                       pattern=#pattern{up = LeftL, right = UpL++UpR,
                                        down = RightR, left = DownR++DownL},
                       count=1, sources=[{IdL, IdR}]}
    end;
match_right_left(_, _) -> false.


match_right_right(#tile{id=IdL, type=center, size={XL, Y},
                        pattern=#pattern{left = LeftL, up = UpL,
                                         right = RightL, down = DownL}},
                  #tile{id=IdR, type=center, size={XR, Y},
                        pattern=#pattern{left = LeftR, up = UpR,
                                         right = RightR, down = DownR}}) ->
    case patterns_match(RightL, RightR) of
        false -> false;
        true  -> #tile{type=center, size={Y, XL+XR},
                       pattern=#pattern{up = LeftL, right = UpL++DownR,
                                        down = LeftR, left = UpR++DownL},
                       count=1, sources=[{IdL, IdR}]}
    end;
match_right_right(_, _) -> false.


match_right_up(#tile{id=IdL, type=center, size={XL, YL},
                     pattern=#pattern{left = LeftL, up = UpL,
                                      right = RightL, down = DownL}},
               #tile{id=IdR, type=center, size={YL, YR},
                     pattern = #pattern{left = LeftR, up = UpR,
                                        right = RightR, down = DownR}}) ->
    case patterns_match(RightL, UpR) of
        false -> false;
        true  -> #tile{type=center, size={YL, XL+YR},
                       pattern=#pattern{up = LeftL, right = UpL++RightR,
                                        down = DownR, left = LeftR++DownL},
                       count=1, sources=[{IdL, IdR}]}
    end;
match_right_up(_, _) -> false.


match_right_down(#tile{id=IdL, type=center, size={XL, YL},
                       pattern=#pattern{left = LeftL, up = UpL,
                                        right = RightL, down = DownL}},
                 #tile{id=IdR, type=center, size={YL, YR},
                       pattern = #pattern{left = LeftR, up = UpR,
                                          right = RightR, down = DownR}}) ->
    case patterns_match(RightL, DownR) of
        false -> false;
        true  -> #tile{type=center, size={YL, XL+YR},
                       pattern=#pattern{up = LeftL, right = UpL++LeftR,
                                        down = UpR, left = RightR++DownL},
                       count=1, sources=[{IdL, IdR}]}
    end;
match_right_down(_, _) -> false.


%% match top pattern on left tile
match_up_left(#tile{id=IdL, type=center, size={XL, YL},
                    pattern=#pattern{left = LeftL, up = UpL,
                                     right = RightL, down = DownL}},
              #tile{id=IdR, type=center, size={XR, XL},
                    pattern = #pattern{left = LeftR, up = UpR,
                                       right = RightR, down = DownR}}) ->
    case patterns_match(UpL, LeftR) of
        false -> false;
        true  -> #tile{type=center, size={XL, YL+XR},
                       pattern=#pattern{up = DownL, right = LeftL++UpR,
                                        down = RightR, left = DownR++RightL},
                       count=1, sources=[{IdL, IdR}]}
    end;
match_up_left(_, _) -> false.


match_up_right(#tile{id=IdL, type=center, size={XL, YL},
                     pattern=#pattern{left = LeftL, up = UpL,
                                      right = RightL, down = DownL}},
               #tile{id=IdR, type=center, size={XR, XL},
                     pattern = #pattern{left = LeftR, up = UpR,
                                        right = RightR, down = DownR}}) ->
    case patterns_match(UpL, RightR) of
        false -> false;
        true  -> #tile{type=center, size={XL, YL+XR},
                       pattern=#pattern{up = DownL, right = LeftL++DownR,
                                        down = LeftR, left = UpR++RightL},
                       count=1, sources=[{IdL, IdR}]}
    end;
match_up_right(_, _) -> false.


match_up_up(#tile{id=IdL, type=center, size={X, YL},
                  pattern=#pattern{left = LeftL, up = UpL,
                                   right = RightL, down = DownL}},
            #tile{id=IdR, type=center, size={X, YR},
                  pattern = #pattern{left = LeftR, up = UpR,
                                     right = RightR, down = DownR}}) ->
    case patterns_match(UpL, UpR) of
        false -> false;
        true  -> #tile{type=center, size={X, YL+YR},
                       pattern=#pattern{up = DownL, right = LeftL++RightR,
                                        down = DownR, left = LeftR++RightL},
                       count=1, sources=[{IdL, IdR}]}
    end;
match_up_up(_, _) -> false.


match_up_down(#tile{id=IdL, type=center, size={X, YL},
                    pattern=#pattern{left = LeftL, up = UpL,
                                     right = RightL, down = DownL}},
              #tile{id=IdR, type=center, size={X, YR},
                    pattern = #pattern{left = LeftR, up = UpR,
                                       right = RightR, down = DownR}}) ->
    case patterns_match(UpL, DownR) of
        false -> false;
        true  -> #tile{type=center, size={X, YL+YR},
                       pattern=#pattern{up = DownL, right = LeftL++LeftR,
                                        down = UpR, left = RightR++RightL},
                       count=1, sources=[{IdL, IdR}]}
    end;
match_up_down(_, _) -> false.


%% match bottom pattern on left tile
match_down_left(#tile{id=IdL, type=center, size={XL, YL},
                      pattern=#pattern{left = LeftL, up = UpL,
                                       right = RightL, down = DownL}},
                #tile{id=IdR, type=center, size={XR, XL},
                      pattern = #pattern{left = LeftR, up = UpR,
                                         right = RightR, down = DownR}}) ->
    case patterns_match(DownL, LeftR) of
        false -> false;
        true  -> #tile{type=center, size={XL, YL+XR},
                       pattern=#pattern{up = UpL, right = RightL++UpR,
                                        down = RightR, left = DownR++LeftL},
                       count=1, sources=[{IdL, IdR}]}
    end;
match_down_left(_, _) -> false.


match_down_right(#tile{id=IdL, type=center, size={XL, YL},
                       pattern=#pattern{left = LeftL, up = UpL,
                                        right = RightL, down = DownL}},
                 #tile{id=IdR, type=center, size={XR, XL},
                       pattern = #pattern{left = LeftR, up = UpR,
                                          right = RightR, down = DownR}}) ->
    case patterns_match(DownL, RightR) of
        false -> false;
        true  -> #tile{type=center, size={XL, YL+XR},
                       pattern=#pattern{up = UpL, right = RightL++DownR,
                                        down = LeftR, left = UpR++LeftL},
                       count=1, sources=[{IdL, IdR}]}
    end;
match_down_right(_, _) -> false.


match_down_up(#tile{id=IdL, type=center, size={X, YL},
                    pattern=#pattern{left = LeftL, up = UpL,
                                     right = RightL, down = DownL}},
              #tile{id=IdR, type=center, size={X, YR},
                    pattern = #pattern{left = LeftR, up = UpR,
                                       right = RightR, down = DownR}}) ->
    case patterns_match(DownL, UpR) of
        false -> false;
        true  -> #tile{type=center, size={X, YL+YR},
                       pattern=#pattern{up = UpL, right = RightL++RightR,
                                        down = DownR, left = LeftR++LeftL},
                       count=1, sources=[{IdL, IdR}]}
    end;
match_down_up(_, _) -> false.


match_down_down(#tile{id=IdL, type=center, size={X, YL},
                      pattern=#pattern{left = LeftL, up = UpL,
                                       right = RightL, down = DownL}},
                #tile{id=IdR, type=center, size={X, YR},
                      pattern = #pattern{left = LeftR, up = UpR,
                                         right = RightR, down = DownR}}) ->
    case patterns_match(DownL, DownR) of
        false -> false;
        true  -> #tile{type=center, size={X, YL+YR},
                       pattern=#pattern{up = UpL, right = RightL++LeftR,
                                        down = UpR, left = RightR++LeftL},
                       count=1, sources=[{IdL, IdR}]}
    end;
match_down_down(_, _) -> false.



patterns_match(P1, P2) when length(P1) /= length(P2) -> false;
patterns_match(P1, P2) -> P1 == lists:reverse(P2).

make_pattern_aux({Left, Down, Right, Up}) ->
    #pattern{left = [Left], up = [Up], right = [Right], down = [Down]};
make_pattern_aux({Left, Down, Right}) ->
    #pattern{left = [Left], right = [Right], down = [Down]};
make_pattern_aux({Right, Down}) ->
    #pattern{right = [Right], down = [Down]}.

sort_pattern(#pattern{up=undefined}=P) -> P;
sort_pattern(#pattern{left=Left, down=Down, right=Right, up=Up}=P0) ->
    P1 = if (Left < Right) orelse
            ((Left == Right) andalso (Down =< Up)) ->
                 P0;
            true ->
                 #pattern{left=Right, down=Up, right=Left, up=Down}
         end,
    if length(Left) /= length(Down) ->
            P1;
       Up < P1#pattern.left andalso Up =< Down ->
            #pattern{left=Up, down=Left, right=Down, up=Right};
       Down < P1#pattern.left ->
            #pattern{left=Down, down=Right, right=Up, up=Left};
       true ->
            P1
    end.
