```erlang



self().

spawn(?MODULE, loop, []).

receive
  Msg -> 
    io:format("Message received ~p~n", [Msg])
end


```