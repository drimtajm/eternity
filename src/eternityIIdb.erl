%%%-------------------------------------------------------------------
%%% @author dreamtime <>
%%% @copyright (C) 2015, dreamtime
%%% @doc
%%%
%%% @end
%%% Created : 15 Feb 2015 by dreamtime <>
%%%-------------------------------------------------------------------
-module(eternityIIdb).

-behaviour(gen_server).

-include("../include/eternityII.hrl").

%% API
-export([start_link/3, stop/0, add_tiles_to_db/1, get_chunk/1, get_chunk/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {current_id  :: pos_integer(),
                dataset1    :: #dataset{},
                dataset2    :: #dataset{},
                results     :: #dataset{},
                index_table :: atom()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link({X1, Y1}, {X2, Y2}, {X, Y}) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
                          [{X1, Y1}, {X2, Y2}, {X, Y}], []).

stop() ->
    Ref = monitor(process, ?SERVER),
    ok = gen_server:call(?SERVER, stop),
    receive
        {'DOWN', Ref, process, _Pid, _Reason} -> ok
    end.

add_tiles_to_db(Pieces) ->
    gen_server:call(?SERVER, {add_to_db, Pieces}).

get_chunk(Type) ->
    gen_server:call(?SERVER, {get_chunk, Type}).

get_chunk(Type, Pattern) ->
    gen_server:call(?SERVER, {get_chunk, Type, Pattern}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([SizePieces1, SizePieces2, Size]) ->
    Dataset1 = mk_dataset(SizePieces1, read),
    Dataset2 = mk_dataset(SizePieces2, read),
    Results  = mk_dataset(Size, read_write),
    IndexTable = mk_index_table(Size),
    {ok, #state{current_id = 1, dataset1 = Dataset1, dataset2 = Dataset2,
                results = Results, index_table = IndexTable}}.

mk_dataset(Size, AccessMode) ->
    MainTable           = mk_table_name(Size),
    CornerPatternsTable = mk_table_name(corner, Size),
    EdgePatternsTable   = mk_table_name(edge, Size),
    CenterPatternsTable = mk_table_name(center, Size),
    open_table(MainTable, set, AccessMode),
    open_table(CornerPatternsTable, bag, AccessMode),
    open_table(EdgePatternsTable, bag, AccessMode),
    open_table(CenterPatternsTable, bag, AccessMode),
    #dataset{main_table      = MainTable,
             corner_patterns = CornerPatternsTable,
             edge_patterns   = EdgePatternsTable,
             center_patterns = CenterPatternsTable,
             tile_size       = Size}.

mk_table_name({X, Y}) ->
    mk_table_name(main, {X, Y}).
mk_table_name(Type, {X, Y}) ->
    list_to_atom(lists:concat([?MODULE, "_", Type, X, x, Y])).

mk_index_table({X, Y}) ->
    Tablename = mk_table_name(index, {X, Y}),
    ets:new(Tablename, [ordered_set, named_table]),
    Tablename.

open_table(Tablename, Type, AccessMode) ->
    dets:open_file(Tablename, [{type, Type}, {access, AccessMode}]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_to_db, Tiles}, _From,
            #state{current_id=CurrentId, index_table=IndexTable,
                   results=Results} = State) ->
    NewId = lists:foldl(
              fun (Tile, Id0) ->
                      add_tile_to_db(Tile, Id0, IndexTable, Results)
              end, CurrentId, Tiles),
    {reply, ok, State#state{current_id=NewId}};
handle_call({get_chunk, Type}, _From,
            #state{dataset2=Dataset2} = State) ->
    {Pattern, Chunk, NewDataset2} = get_chunk_aux(Type, Dataset2),
    Reply = case Chunk of
                no_data_left -> no_data_left;
                _Else        -> {ok, Pattern, Chunk}
            end,
    {reply, Reply, State#state{dataset2=NewDataset2}};
handle_call({get_chunk, Type, Pattern}, _From,
            #state{dataset1=Dataset1} = State) ->
    {Pattern, Chunk, NewDataset1} = get_chunk_aux(Type, Pattern, Dataset1),
    Reply = case Chunk of
                no_data_left -> no_data_left;
                _Else        -> {ok, Chunk}
            end,
    {reply, Reply, State#state{dataset1=NewDataset1}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

add_tile_to_db(#tile{type=Type, pattern=Pattern0,
                      sources=[Source0]} = Tile0, Id0, IndexTable,
                Results) ->
    Pattern = eternityII_lib:sort_pattern(Pattern0),
    Key = {Type, Pattern},
    case ets:lookup(IndexTable, Key) of
        [] ->
            Size = case get_size(Pattern) of
                       old_size -> Tile0#tile.size;
                       Size0    -> Size0
                   end,
            Tile = Tile0#tile{id = Id0, pattern=Pattern, size=Size},
            add_new_tile_to_db(Tile, Results),
            ets:insert(IndexTable, {Key, Id0}),
            Id0+1;
        [{Key, Id}] ->
            update_tile_in_db(Tile0, Id, Results),
            Id0
    end.

get_size(#pattern{right=Right, down=Down}) when Right /= undefined
                                                andalso Down /= undefined ->
    {length(Down), length(Right)};
get_size(_) -> old_size.

add_new_tile_to_db(#tile{id=Id, type=corner,
                         pattern={righ=P1, down=P2}}=Tile,
                   #dataset{main_table=MainTablename,
                            corner_patterns=CornerPatternsTablename}) ->
    ok = dets:insert(MainTablename, {Id, Tile}),
    ok = dets:insert(CornerPatternsTablename, {P1, Id}),
    ok = dets:insert(CornerPatternsTablename, {P2, Id});
add_new_tile_to_db(#tile{id=Id, type=edge,
                         pattern={righ=P1, down=P2, left=P3}}=Tile,
                   #dataset{main_table=MainTablename,
                            edge_patterns=EdgePatternsTablename}) ->
    ok = dets:insert(MainTablename, {Id, Tile}),
    ok = dets:insert(EdgePatternsTablename, {P1, Id}),
    ok = dets:insert(EdgePatternsTablename, {P2, Id}),
    ok = dets:insert(EdgePatternsTablename, {P3, Id});
add_new_tile_to_db(#tile{id=Id, type=center,
                         pattern={righ=P1, down=P2, left=P3, up=P4}}=Tile,
                   #dataset{main_table=MainTablename,
                            center_patterns=CenterPatternsTablename}) ->
    ok = dets:insert(MainTablename, {Id, Tile}),
    ok = dets:insert(CenterPatternsTablename, {P1, Id}),
    ok = dets:insert(CenterPatternsTablename, {P2, Id}),
    ok = dets:insert(CenterPatternsTablename, {P3, Id}),
    ok = dets:insert(CenterPatternsTablename, {P4, Id}).

update_tile_in_db(#tile{sources=[Source0], primary=PrimarySources0}, Id,
                  #dataset{main_table=MainTablename}) ->
    [#tile{sources=OldSources, count=Count,
           primary=OldPrimarySources}=Tile0] = dets:lookup(MainTablename, Id),
    case lists:member(Source0, OldSources) of
        true  -> nothing_to_do;
        false ->
            NewSources = lists:sort([Source0 | OldSources]),
            NewPrimarySources = lists:sort(OldPrimarySources
                                           ++ PrimarySources0),
            Tile = Tile0#tile{count=Count+1,
                              sources=NewSources,
                              primary=NewPrimarySources},
            ok = dets:insert(MainTablename, {Id, Tile})
    end.

get_chunk_from_db('$end_of_table', _, _) ->
    no_data_left;
get_chunk_from_db(Key, Table, Continue) ->
    case dets:match(Table, {Key, '$1'}, 50) of
        '$end_of_table' ->
            if (Continue == true) ->
                    get_chunk_from_db(dets:next(Table, Key), Table, Continue);
               true -> no_data_left
            end;
        {Ids, Continuation} when is_list(Ids) ->
            {Key, Ids, Continuation}
    end.

get_chunk_from_db(Key, Table, Continue, Continuation) ->
    case dets:match(Continuation) of
        '$end_of_table' ->
            if (Continue == true) ->
                    get_chunk_from_db(dets:next(Table, Key), Table, Continue);
               true -> no_data_left
            end;
        {Ids, NewContinuation} when is_list(Ids) ->
            {Key, Ids, NewContinuation}
    end.

lookup_tiles_from_ids(Ids, #dataset{main_table=MainTable}) ->
    lists:map(fun (Id) ->
                      [Tile] = dets:lookup(MainTable, Id), Tile
              end, Ids).

get_table_by_type(corner, #dataset{corner_patterns=CornerPatterns}) ->
    CornerPatterns;
get_table_by_type(edge, #dataset{edge_patterns=EdgePatterns}) ->
    EdgePatterns;
get_table_by_type(center, #dataset{center_patterns=CenterPatterns}) ->
    CenterPatterns.

get_chunk_aux(Type, #dataset{current_type=CType} = Dataset)
  when CType /= Type ->
    Table = get_table_by_type(Type, Dataset),
    CurrentKey = dets:first(Table),
    get_chunk_aux(Type, CurrentKey, Table, Dataset, true);
get_chunk_aux(Type, #dataset{current_type=Type, db_pointer=Continuation,
                             current_pattern=CurrentKey} = Dataset) ->
    Table = get_table_by_type(Type, Dataset),
    get_chunk_aux(Type, CurrentKey, Table, Dataset, true, Continuation).

get_chunk_aux(Type, Key, #dataset{current_pattern=CKey} = Dataset)
  when CKey /= Key ->
    Table = get_table_by_type(Type, Dataset),
    get_chunk_aux(Type, Key, Table, Dataset, false);
get_chunk_aux(Type, Key, #dataset{current_type=Type, db_pointer=Continuation,
                                  current_pattern=Key} = Dataset) ->
    Table = get_table_by_type(Type, Dataset),
    get_chunk_aux(Type, Key, Table, Dataset, false, Continuation).


get_chunk_aux(Type, CurrentKey, Table, Dataset, Continue) ->
    case get_chunk_from_db(CurrentKey, Table, Continue) of
        no_data_left ->
            {CurrentKey, no_data_left,
             Dataset#dataset{current_pattern=undefined}};
        {Key, Ids, Continuation} ->
            Chunk = lookup_tiles_from_ids(Ids, Dataset),
            {Key, Chunk, Dataset#dataset{current_type=Type,
                                         current_pattern=Key,
                                         db_pointer=Continuation}}
    end.
get_chunk_aux(Type, CurrentKey, Table, Dataset, Continue, Continuation) ->
    case get_chunk_from_db(CurrentKey, Table, Continue, Continuation) of
        no_data_left ->
            {CurrentKey, no_data_left,
             Dataset#dataset{current_pattern=undefined}};
        {Key, Ids, NewContinuation} ->
            Chunk = lookup_tiles_from_ids(Ids, Dataset),
            {Key, Chunk, Dataset#dataset{current_type=Type,
                                         current_pattern=Key,
                                         db_pointer=NewContinuation}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{dataset1=Dataset1, dataset2=Dataset2,
                          results=Results, index_table=IndexTable}=_State) ->
    cleanup_dataset(Dataset1),
    cleanup_dataset(Dataset2),
    cleanup_dataset(Results),
    cleanup_index_table(IndexTable),
    ok.

cleanup_dataset(#dataset{main_table=MainTable,
                         corner_patterns=CornerPatternsTable,
                         edge_patterns=EdgePatternsTable,
                         center_patterns=CenterPatternsTable}) ->
    ok = close_table(MainTable),
    ok = close_table(CornerPatternsTable),
    ok = close_table(EdgePatternsTable),
    ok = close_table(CenterPatternsTable).

close_table(Tablename) ->
    dets:close(Tablename).

cleanup_index_table(IndexTable) ->
    true = ets:delete(IndexTable).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
