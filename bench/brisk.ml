type node = unit
let insertNode ~parent  ~child:_  ~position:_  = parent
let deleteNode ~parent  ~child:_  ~position:_  = parent
let moveNode ~parent  ~child:_  ~from:_  ~to_:_  = parent
let beginChanges () = ()
let commitChanges () = ()
include Brisk_reconciler