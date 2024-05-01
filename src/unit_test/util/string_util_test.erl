-module(string_util_test).

-include_lib("eunit/include/eunit.hrl").


trim_test() ->
	[
	 	?assert(string_util:trim("echoooo") == string_util:trim(<<"echoooo">>)),
		?assert(string_util:trim("echoooo") == <<"echoooo">>),
		?assert(string_util:trim(" echoooo ") =:= string_util:trim(<<" echoooo">>)),
		?assert(string_util:trim(" echoooo ") =:= <<"echoooo">>),
		?assert(string_util:trim("echoooo ") =:= string_util:trim(<<"echoooo ">>)),
		?assert(string_util:trim("echoooo ") =:= <<"echoooo">>),
		
		?assert(string_util:trim(<<"echoooo">>) == string_util:trim(<<"echoooo">>)),
		?assert(string_util:trim(<<"echoooo">>) == <<"echoooo">>),
		?assert(string_util:trim(<<" echoooo ">>) =:= string_util:trim(<<" echoooo">>)),
		?assert(string_util:trim(<<" echoooo ">>) =:= <<"echoooo">>),
		?assert(string_util:trim(<<"echoooo ">>) =:= string_util:trim(<<"echoooo ">>)),
		?assert(string_util:trim(<<"echoooo ">>) =:= <<"echoooo">>),
		
		?assert(string_util:trim(123) =:= <<"123">>),
		?assert(string_util:trim(1237856533) =:= <<"1237856533">>),
		?assert(string_util:trim(-123) =:= <<"-123">>),
		?assert(string_util:trim(-1237856533) =:= <<"-1237856533">>),
		?assert(string_util:trim(1) =:= <<"1">>),
		?assert(string_util:trim(-1) =:= <<"-1">>),
		?assert(string_util:trim(1.1) =:= <<"1.1">>),
		?assert(string_util:trim(-1.1) =:= <<"-1.1">>),
		?assert(string_util:trim(1.10) =:= <<"1.1">>),
		?assert(string_util:trim(-1.10) =:= <<"-1.1">>),
		?assert(string_util:trim(1.12) =:= <<"1.12">>),
		?assert(string_util:trim(-1.12) =:= <<"-1.12">>),
		
		?assert(string_util:trim(nil) =:= string_util:trim(" ")),
		?assert(string_util:trim([]) =:= string_util:trim(" ")),
		
		?assert(string_util:trim([$ ,$a,$b,$ ,$c,$ ,$ ]) =:= string_util:trim(<<" ab c  ">>)),
		?assert(string_util:trim([$ ,$a,$b,$ ,$c,$ ,$ ]) =:= <<"ab c">>),
		
		?assert(string_util:trim([$ ,$a,$1,$b,$2,$ ,$c,$ ,$ ]) =:= string_util:trim(<<" a1b2 c  ">>)),
		?assert(string_util:trim([$ ,$a,$1,$b,$2,$ ,$c,$ ,$ ]) =:= <<"a1b2 c">>),
		
		?assert(string_util:trim(true) =:= <<>>),
		?assert(string_util:trim(false) =:= <<>>),
		
		?assert(string_util:trim(another_atom) =:= <<>>),
		?assert(string_util:trim(another_atom) =:= <<>>)
    ]
	.