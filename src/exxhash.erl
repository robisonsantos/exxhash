-module(exxhash).
-export([xx32/2]).
-export([xx32_init/1, 
         xx32_update/2, 
         xx32_digest/1]).

%%-------------------------------------------------
%% Special swap function
%%-------------------------------------------------
-define(XXH_swap32, fun(X) when is_integer(X) -> 
        ((X bsl 24) band 16#ff000000) bor
        ((X bsl  8) band 16#00ff0000) bor
        ((X bsr  8) band 16#0000ff00) bor
        ((X bsr 24) band 16#000000ff)
    end).
  
%%-------------------------------------------------
%% Special rotate function
%%-------------------------------------------------
-define(XXH_rotl32, fun(X,R) ->
       ((X bsl R) bor (X bsr (32 - R)))
    end).

%%-------------------------------------------------
%% Constants
%%-------------------------------------------------
-define(PRIME32_1, 2654435761).
-define(PRIME32_2, 2246822519).
-define(PRIME32_3, 3266489917).
-define(PRIME32_4,  668265263).
-define(PRIME32_5,  374761393).

%%-------------------------------------------------
%% XXH Structure
%%-------------------------------------------------
-record(xxh, {seed, v1, v2, v3, v4, total_len, memory, memsize}).

%%------------------------------------------------
%% FUNCTIONS
%%------------------------------------------------
-spec xx32_init(integer()) -> {ok, #xxh{}}.
xx32_init(Seed) -> {ok, reset_state(Seed)}.

-spec xx32_update(string() | binary(), integer()) -> {ok, #xxh{}} | {error, undefined}.
xx32_update(undefined, _Seed) -> 
  {error, undefined};
xx32_update(Input, Seed) when is_list(Input) -> 
  xx32_update(list_to_binary(Input), Seed);
xx32_update(Input, Seed) when is_binary(Input),
                              is_integer(Seed) -> 
  ok.

-spec xx32_digest(#xxh{}) -> {ok, integer()}.
xx32_digest(#xxh{} = _State) -> ok.

-spec xx32(string() | binary(), integer()) -> {ok, integer()}.
xx32(Input, Seed) when is_list(Input) -> 
  xx32(list_to_binary(Input), Seed);
xx32(Input, Seed) when is_binary(Input), is_integer(Seed) ->
  InputSize = size(Input),
  {P1, H32_p1} = calc_h32_p1(InputSize - 16, Seed),
  {P2, H32_p2} = calc_h32_p2(InputSize - 4, P1, H32_p1),
  H32_p3 = calc_h32_p3(InputSize, P2, H32_p2),

  %%-------------------
  %% h32 ^= h32 >> 15;
  %% h32 *= PRIME32_2;
  %% h32 ^= h32 >> 13;
  %% h32 *= PRIME32_3;
  %% h32 ^= h32 >> 16;
  %% ------------------
  H32_p4 = H32_p3 bxor (H32_p3 bsr 15),
  H32_p5 = H32_p4 * ?PRIME32_2,
  H32_p6 = H32_p5 bxor (H32_p5 bsr 13),
  H32_p7 = H32_p6 * ?PRIME32_3,
  H32_p8 = H32_p7 bxor (H32_p7 bsr 16),

  H32_p8.

%%-------------------------------------------------
%% Helper functions
%%-------------------------------------------------
calc_h32_p1(InputSize, Seed) when InputSize < 16 ->
  {0, Seed + ?PRIME32_5 + InputSize};
calc_h32_p1(InputSize, Seed) ->
  {P, {V1, V2, V3, V4}} = calc_intermediate(InputSize - 16, 
                                       0, 
                                      {Seed + ?PRIME32_1 + ?PRIME32_2,
                                       Seed + ?PRIME32_2,
                                       Seed,
                                       Seed - ?PRIME32_1}),
  H32 = ?XXH_rotl32(V1, 1)  + 
        ?XXH_rotl32(V2, 7)  + 
        ?XXH_rotl32(V3, 12) + 
        ?XXH_rotl32(V4, 18) + 
        InputSize,
  {P, H32}.

%%------------------------------------------------------------------------
calc_h32_p2(Limit, P, H32) when P > Limit -> 
  {P, H32};
calc_h32_p2(Limit, P, H32) ->
  calc_h32_p2(Limit, 
              P + 4, 
              ?XXH_rotl32(H32 + (?XXH_swap32(P) * ?PRIME32_3), 11) * ?PRIME32_4).


calc_h32_p3(Limit, P, H32) when P >= Limit ->
  H32;
calc_h32_p3(Limit, P, H32) ->
  calc_h32_p3(Limit, P + 1, ?XXH_rotl32(H32 + (P * ?PRIME32_5), 11) * ?PRIME32_1).


%%------------------------------------------------------------------------
calc_intermediate(Limit, P, Ret) when P =< Limit ->
  {P, Ret}; 
calc_intermediate(Limit, P, {V1, V2, V3, V4}) ->
  {NV1, PV1} = get_value(V1, P),
  {NV2, PV2} = get_value(V2, PV1),
  {NV3, PV3} = get_value(V3, PV2),
  {NV4, PV4} = get_value(V4, PV3),

  calc_intermediate(Limit, PV4, {NV1, NV2, NV3, NV4}).

%%------------------------------------------------------------------------
get_value(V, P) ->
  {?XXH_rotl32(V + ?XXH_swap32(P), 13) * ?PRIME32_1, P + 4}.

%%------------------------------------------------------------------------
reset_state(Seed) ->
  #xxh{seed = Seed,
       v1 = Seed + ?PRIME32_1 + ?PRIME32_2,
       v2 = Seed + ?PRIME32_3,
       v3 = Seed,
       v4 = Seed - ?PRIME32_1,
       total_len = 0,
       memsize = 0}.

