`./rebar get-deps`
`./rebar compile`

this works well

`sh test/work_well.sh`

this hangs

`sh test/not_work_well.sh`


what is #Fun<async_t.10.42891525>

see [async_t:promise](https://github.com/slepher/async/blob/master/src/async_t.erl#L180)

do a gen_server:call and save mref and the callback to state

execute callback while get reply of previous gen_server:call

#Fun<async_t.10.42891525> is that callback

type of #Fun<async_t.10.42891525> is fun((A) -> async_r_m(S, R)).
async_r_m(S, R) :: fun((S) -> fun((reference()) -> fun((callback_gs(S)) -> {S, A})).
callback_gs(S) :: {fun((S) -> #{reference() => Val}), fun((#{reference() => Val}, S) -> S)}.
