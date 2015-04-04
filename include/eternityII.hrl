-record(pattern, {left  :: list() | undefined,
                  up    :: list() | undefined,
                  right :: list() | undefined,
                  down  :: list() | undefined}).
-record(tile, {id       :: pos_integer(),
               type     :: atom(),
               pattern  :: #pattern{},
               size     :: {pos_integer(), pos_integer()},
               count    :: pos_integer(),
               sources  :: list(),
               primary  :: list()}).
-record(dataset, {main_table      :: atom(),
                  corner_patterns :: atom(),
                  edge_patterns   :: atom(),
                  center_patterns :: atom(),
                  current_type    :: atom(),
                  current_pattern :: list(),
                  db_pointer      :: term(),
                  tile_size       :: {pos_integer(), pos_integer()}}).

