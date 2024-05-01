-module(transaction).

-export([new/6, is_valid_transaction/1, calculate_hash/5]).

new(Map, Sender, Receiver, Amount, Key, SenderPublicKey) -> 
	Time = date_util:now(0),
	Map2 = maps:put(<<"sender">>, Sender, Map),
	Map3 = maps:put(<<"receiver">>, Receiver, Map2),
	Map4 = maps:put(<<"amount">>, Amount, Map3),
	Map5 = maps:put(<<"time">>, Time, Map4),
	Map6 = maps:put(<<"keyy">>, Key, Map5),
	Map7 = maps:put(<<"hash">>, calculate_hash(Sender, Receiver, Amount, Time, Key), Map6),
	sign_transaction(Map7, SenderPublicKey).

is_valid_transaction(Map) ->
	case (undefined == Map) of
		true ->
			false;
		_ ->
			Hash = maps:get(<<"hash">>, Map),
			Sender = maps:get(<<"sender">>, Map),
			Receiver = maps:get(<<"receiver">>, Map),
			Amount = maps:get(<<"amount">>, Map),
			Time = maps:get(<<"time">>, Map),
			Key = maps:get(<<"keyy">>, Map),
			CalculatedHash = calculate_hash(Sender, Receiver, Amount, Time, Key),
			if
				(Hash /= CalculatedHash) -> false;
				(Sender == Receiver) -> false;
				true -> validate_signature(Map)
			end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calculate_hash(Sender, Receiver, Amount, Time, Key) ->
	HashString = lists:concat([
							   blockchain_util:to_concat(Sender), 
							   blockchain_util:to_concat(Receiver), 
							   Amount, 
							   Time
							  ]),
	HashEncoded = blockchain_util:encode_hashstring(HashString),
	blockchain_util:crypto256Hex(Key, HashEncoded).

sign_transaction(Map, SenderPublicKey) ->
	Hash = maps:get(<<"hash">>, Map),
	Sender = maps:get(<<"sender">>, Map),
	Receiver = maps:get(<<"receiver">>, Map),
	Amount = maps:get(<<"amount">>, Map),
	Time = maps:get(<<"time">>, Map),
	Key = maps:get(<<"keyy">>, Map),
	CalculatedHash = calculate_hash(Sender, Receiver, Amount, Time, Key),
	PublicKey = blockchain_util:read_public_key(),
	PrivateKey = blockchain_util:read_private_key(),
	if
		(Hash /= CalculatedHash) -> undefined;
		(Sender == Receiver) -> undefined;
		(undefined == PublicKey) -> undefined;
		(undefined == SenderPublicKey) -> undefined;
		true -> 
			InvalidSenderKey = string:length(string_util:trim(SenderPublicKey)) == 0,
			case (InvalidSenderKey) of 
				true ->
					undefined;
				_ ->
					sign_transaction2(Map, PrivateKey, PublicKey, SenderPublicKey)
			end
	end.

sign_transaction2(Map, PrivateKey, PublicKey, SenderPublicKey) ->
	SenderSignature = hash_util:hash_password(SenderPublicKey),
	HashSignature = blockchain_util:sign_with_private_key(PrivateKey, SenderPublicKey),
	Map2 = maps:put(<<"sender_signature">>, SenderSignature, Map),
	Map3 = maps:put(<<"hash_signature">>, HashSignature, Map2),
	Map4 = maps:put(<<"public_key">>, PublicKey, Map3),
	maps:put(<<"sender_public_key">>, SenderPublicKey, Map4).

validate_signature(Map) ->
	SenderSignature = maps:get(<<"sender_signature">>, Map),
	HashSignature = maps:get(<<"hash_signature">>, Map),
	PublicKey = maps:get(<<"public_key">>, Map),
	SenderPublicKey = maps:get(<<"sender_public_key">>, Map),
	InvalidSenderKey = string:length(string_util:trim(SenderPublicKey)) == 0,
	case (InvalidSenderKey) of 
		true ->
			false;
		_ ->
			Ok1 = hash_util:password_match(SenderSignature, SenderPublicKey),
			Ok2 = blockchain_util:verify_with_public_key(PublicKey, HashSignature, SenderPublicKey),
			Ok1 and Ok2
	end.
