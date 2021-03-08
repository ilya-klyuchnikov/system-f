val make_check_equal :
  ?test_module:string ->
  ?to_string:('a -> string) ->
  unit ->
  ?name:string ->
  (unit -> 'a) ->
  'a ->
  unit
