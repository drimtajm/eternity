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
