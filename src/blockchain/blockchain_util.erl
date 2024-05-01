-module(blockchain_util).

-export(
   		[
		 generate_keys/0,
		 read_private_key/0,
		 read_public_key/0,
		 sign_with_private_key/2,
		 verify_with_public_key/3,
		 encode_hashstring/1, 
		 crypto256Hex/2, 
		 full_array_int/1, 
		 to_concat/1, 
		 split_transactions/6
		]
	   ).

-define(PUBLIC_KEY_FILE, "/var/www/html/cryptominer/blockchain_public.pem").
-define(PRIVATE_KEY_FILE, "/var/www/html/cryptominer/blockchain_private.pem").

generate_keys() ->
	Command1 = lists:concat([
							 "openssl genrsa -out ", 
							 ?PRIVATE_KEY_FILE, 
							 " 2048"
							]),
	Command2 = lists:concat([
							 "openssl rsa -in ", 
							 ?PRIVATE_KEY_FILE, 
							 " -out ", 
							 ?PUBLIC_KEY_FILE, 
							 " -outform PEM -pubout"
							]),	
	os:cmd(Command1),
	os:cmd(Command2),
	ok.

read_private_key() ->
	PrivateKey = file_util:read_file(?PRIVATE_KEY_FILE),
	case (undefined == PrivateKey) of
		true ->
			generate_keys(),
			timer:sleep(1),
			read_private_key();
		_ ->
			PrivateKey
	end.

read_public_key() ->
	PublicKey = file_util:read_file(?PUBLIC_KEY_FILE),
	case (undefined == PublicKey) of
		true ->
			generate_keys(),
			timer:sleep(1),
			read_public_key();
		_ ->
			PublicKey
	end.

sign_with_private_key(PrivateKey, ClearMessage) ->
	[PrivateKey2] = public_key:pem_decode(PrivateKey),
	PrivateKey3 = public_key:pem_entry_decode(PrivateKey2),
	public_key:sign(ClearMessage, sha256, PrivateKey3).

verify_with_public_key(PublicKey, SignedMessage, ExpectedClearMessage) ->
	[PublicKey2] = public_key:pem_decode(PublicKey),
	PublicKey3 = public_key:pem_entry_decode(PublicKey2),
	public_key:verify(ExpectedClearMessage, sha256, SignedMessage, PublicKey3).

encode_hashstring(HashString) ->
	PropList = [{hashString, HashString}],
	Map = maps:from_list(PropList),
	json_util:encode_map(Map).

crypto256Hex(Key, HashEncoded) ->
	crypto:mac(hmac, sha256, Key, HashEncoded).

full_array_int(Length) ->
	full_array_int2([], 1, Length).

to_concat(String) ->
	case (is_binary(String) or is_bitstring(String)) of
		true ->
			binary_to_list(String);
		_ ->
			String
	end.

split_transactions(Transactions, Transactions2, Start, Limit, Count, Length) ->
	case (Count > Limit) or (Count > Length) of
		true ->
			lists:reverse(Transactions2);
		_ ->
			Transactions3 = case (Count < Start) of
								true -> 
									Transactions2;
								_ ->
									[lists:nth(Count, Transactions) | Transactions2]
							end,
			split_transactions(Transactions, Transactions3, Start, Limit, Count + 1, Length)	
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

full_array_int2(Array, Counter, Length) ->
	case (Counter > Length) of
		true -> 
			lists:reverse(Array);
		_ ->
			full_array_int2([Counter | Array], Counter + 1, Length)
	end.




















