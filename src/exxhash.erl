-module(exxhash).
-compile(export_all).

%%-------------------------------------------------
%% Special swap function
%%-------------------------------------------------
-define(XXH_swap32, fun(X) when is_integer(X) -> 
        ((X bsl 24) band 0xff000000) bor
        ((X bsl  8) band 0x00ff0000) bor
        ((X bsr  8) band 0x0000ff00) bor
        ((X bsr 24) band 0x000000ff)
    end).
    
%%-------------------------------------------------
%% Constants
%%-------------------------------------------------
-define(PRIME32_1, 2654435761).
-define(PRIME32_2, 2246822519).
-define(PRIME32_3, 3266489917).
-define(PRIME32_4,  668265263).
-define(PRIME32_5,  374761393).


