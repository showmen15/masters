-file("src/roboclaw_pb.erl", 1).

-module(roboclaw_pb).

-export([encode_motorsspeed/1, decode_motorsspeed/1,
	 delimited_decode_motorsspeed/1, encode_drivermsg/1,
	 decode_drivermsg/1, delimited_decode_drivermsg/1,
	 encode_driverhdr/1, decode_driverhdr/1,
	 delimited_decode_driverhdr/1]).

-export([has_extension/2, extension_size/1,
	 get_extension/2, set_extension/3]).

-export([decode_extensions/1]).

-export([encode/1, decode/2, delimited_decode/2]).

-record(motorsspeed,
	{frontleftspeed, frontrightspeed, rearleftspeed,
	 rearrightspeed}).

-record(drivermsg,
	{type, synnum, acknum, listenernum, '$extensions'}).

-record(driverhdr, {devicetype, deviceid, clientids}).

encode([]) -> [];
encode(Records) when is_list(Records) ->
    delimited_encode(Records);
encode(Record) -> encode(element(1, Record), Record).

encode_motorsspeed(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_motorsspeed(Record)
    when is_record(Record, motorsspeed) ->
    encode(motorsspeed, Record).

encode_drivermsg(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_drivermsg(Record)
    when is_record(Record, drivermsg) ->
    encode(drivermsg, Record).

encode_driverhdr(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_driverhdr(Record)
    when is_record(Record, driverhdr) ->
    encode(driverhdr, Record).

encode(driverhdr, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(driverhdr, Record) ->
    [iolist(driverhdr, Record) | encode_extensions(Record)];
encode(drivermsg, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(drivermsg, Record) ->
    [iolist(drivermsg, Record) | encode_extensions(Record)];
encode(motorsspeed, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(motorsspeed, Record) ->
    [iolist(motorsspeed, Record)
     | encode_extensions(Record)].

encode_extensions(#drivermsg{'$extensions' =
				 Extends}) ->
    [pack(Key, Optionalness, Data, Type, Accer)
     || {Key, {Optionalness, Data, Type, Accer}}
	    <- dict:to_list(Extends)];
encode_extensions(_) -> [].

delimited_encode(Records) ->
    lists:map(fun (Record) ->
		      IoRec = encode(Record),
		      Size = iolist_size(IoRec),
		      [protobuffs:encode_varint(Size), IoRec]
	      end,
	      Records).

iolist(driverhdr, Record) ->
    [pack(1, optional,
	  with_default(Record#driverhdr.devicetype, none), int32,
	  []),
     pack(2, optional,
	  with_default(Record#driverhdr.deviceid, none), int32,
	  []),
     pack(3, repeated_packed,
	  with_default(Record#driverhdr.clientids, none), int32,
	  [])];
iolist(drivermsg, Record) ->
    [pack(2, required,
	  with_default(Record#drivermsg.type, none),
	  drivermsg_msgtype, []),
     pack(3, optional,
	  with_default(Record#drivermsg.synnum, none), uint32,
	  []),
     pack(4, optional,
	  with_default(Record#drivermsg.acknum, none), uint32,
	  []),
     pack(5, optional,
	  with_default(Record#drivermsg.listenernum, none),
	  uint32, [])];
iolist(motorsspeed, Record) ->
    [pack(1, optional,
	  with_default(Record#motorsspeed.frontleftspeed, none),
	  int32, []),
     pack(2, optional,
	  with_default(Record#motorsspeed.frontrightspeed, none),
	  int32, []),
     pack(3, optional,
	  with_default(Record#motorsspeed.rearleftspeed, none),
	  int32, []),
     pack(4, optional,
	  with_default(Record#motorsspeed.rearrightspeed, none),
	  int32, [])].

with_default(Default, Default) -> undefined;
with_default(Val, _) -> Val.

pack(_, optional, undefined, _, _) -> [];
pack(_, repeated, undefined, _, _) -> [];
pack(_, repeated_packed, undefined, _, _) -> [];
pack(_, repeated_packed, [], _, _) -> [];
pack(FNum, required, undefined, Type, _) ->
    exit({error,
	  {required_field_is_undefined, FNum, Type}});
pack(_, repeated, [], _, Acc) -> lists:reverse(Acc);
pack(FNum, repeated, [Head | Tail], Type, Acc) ->
    pack(FNum, repeated, Tail, Type,
	 [pack(FNum, optional, Head, Type, []) | Acc]);
pack(FNum, repeated_packed, Data, Type, _) ->
    protobuffs:encode_packed(FNum, Data, Type);
pack(FNum, _, Data, _, _) when is_tuple(Data) ->
    [RecName | _] = tuple_to_list(Data),
    protobuffs:encode(FNum, encode(RecName, Data), bytes);
pack(FNum, _, Data, Type, _)
    when Type =:= bool;
	 Type =:= int32;
	 Type =:= uint32;
	 Type =:= int64;
	 Type =:= uint64;
	 Type =:= sint32;
	 Type =:= sint64;
	 Type =:= fixed32;
	 Type =:= sfixed32;
	 Type =:= fixed64;
	 Type =:= sfixed64;
	 Type =:= string;
	 Type =:= bytes;
	 Type =:= float;
	 Type =:= double ->
    protobuffs:encode(FNum, Data, Type);
pack(FNum, _, Data, Type, _) when is_atom(Data) ->
    protobuffs:encode(FNum, enum_to_int(Type, Data), enum).

enum_to_int(drivermsg_msgtype, 'UNSUBSCRIBE') -> 7;
enum_to_int(drivermsg_msgtype, 'SUBSCRIBE') -> 6;
enum_to_int(drivermsg_msgtype, 'DRIVER_DIED') -> 5;
enum_to_int(drivermsg_msgtype, 'CLIENT_DIED') -> 4;
enum_to_int(drivermsg_msgtype, 'PONG') -> 3;
enum_to_int(drivermsg_msgtype, 'PING') -> 2;
enum_to_int(drivermsg_msgtype, 'DATA') -> 1;
enum_to_int(devicetype, 'MAESTRO') -> 7;
enum_to_int(devicetype, 'LOCATION') -> 6;
enum_to_int(devicetype, 'DUMMY') -> 5;
enum_to_int(devicetype, 'HOKUYO') -> 4;
enum_to_int(devicetype, 'STARGAZER') -> 3;
enum_to_int(devicetype, 'ROBOCLAW') -> 2;
enum_to_int(devicetype, 'NINEDOF') -> 1.

int_to_enum(drivermsg_msgtype, 7) -> 'UNSUBSCRIBE';
int_to_enum(drivermsg_msgtype, 6) -> 'SUBSCRIBE';
int_to_enum(drivermsg_msgtype, 5) -> 'DRIVER_DIED';
int_to_enum(drivermsg_msgtype, 4) -> 'CLIENT_DIED';
int_to_enum(drivermsg_msgtype, 3) -> 'PONG';
int_to_enum(drivermsg_msgtype, 2) -> 'PING';
int_to_enum(drivermsg_msgtype, 1) -> 'DATA';
int_to_enum(devicetype, 7) -> 'MAESTRO';
int_to_enum(devicetype, 6) -> 'LOCATION';
int_to_enum(devicetype, 5) -> 'DUMMY';
int_to_enum(devicetype, 4) -> 'HOKUYO';
int_to_enum(devicetype, 3) -> 'STARGAZER';
int_to_enum(devicetype, 2) -> 'ROBOCLAW';
int_to_enum(devicetype, 1) -> 'NINEDOF';
int_to_enum(_, Val) -> Val.

decode_motorsspeed(Bytes) when is_binary(Bytes) ->
    decode(motorsspeed, Bytes).

decode_drivermsg(Bytes) when is_binary(Bytes) ->
    decode(drivermsg, Bytes).

decode_driverhdr(Bytes) when is_binary(Bytes) ->
    decode(driverhdr, Bytes).

delimited_decode_driverhdr(Bytes) ->
    delimited_decode(driverhdr, Bytes).

delimited_decode_drivermsg(Bytes) ->
    delimited_decode(drivermsg, Bytes).

delimited_decode_motorsspeed(Bytes) ->
    delimited_decode(motorsspeed, Bytes).

delimited_decode(Type, Bytes) when is_binary(Bytes) ->
    delimited_decode(Type, Bytes, []).

delimited_decode(_Type, <<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
delimited_decode(Type, Bytes, Acc) ->
    try protobuffs:decode_varint(Bytes) of
      {Size, Rest} when size(Rest) < Size ->
	  {lists:reverse(Acc), Bytes};
      {Size, Rest} ->
	  <<MessageBytes:Size/binary, Rest2/binary>> = Rest,
	  Message = decode(Type, MessageBytes),
	  delimited_decode(Type, Rest2, [Message | Acc])
    catch
      _What:_Why -> {lists:reverse(Acc), Bytes}
    end.

decode(enummsg_values, 1) -> value1;
decode(driverhdr, Bytes) when is_binary(Bytes) ->
    Types = [{3, clientids, int32, [repeated_packed]},
	     {2, deviceid, int32, []}, {1, devicetype, int32, []}],
    Defaults = [{3, clientids, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(driverhdr, Decoded);
decode(drivermsg, Bytes) when is_binary(Bytes) ->
    Types = [{5, listenernum, uint32, []},
	     {4, acknum, uint32, []}, {3, synnum, uint32, []},
	     {2, type, drivermsg_msgtype, []}],
    Defaults = [{false, '$extensions',
		 {dict, 0, 16, 16, 8, 80, 48,
		  {[], [], [], [], [], [], [], [], [], [], [], [], [], [],
		   [], []},
		  {{[], [], [], [], [], [], [], [], [], [], [], [], [],
		    [], [], []}}}}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(drivermsg, Decoded);
decode(motorsspeed, Bytes) when is_binary(Bytes) ->
    Types = [{4, rearrightspeed, int32, []},
	     {3, rearleftspeed, int32, []},
	     {2, frontrightspeed, int32, []},
	     {1, frontleftspeed, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(motorsspeed, Decoded).

decode(<<>>, Types, Acc) ->
    reverse_repeated_fields(Acc, Types);
decode(Bytes, Types, Acc) ->
    {ok, FNum} = protobuffs:next_field_num(Bytes),
    case lists:keyfind(FNum, 1, Types) of
      {FNum, Name, Type, Opts} ->
	  {Value1, Rest1} = case lists:member(is_record, Opts) of
			      true ->
				  {{FNum, V}, R} = protobuffs:decode(Bytes,
								     bytes),
				  RecVal = decode(Type, V),
				  {RecVal, R};
			      false ->
				  case lists:member(repeated_packed, Opts) of
				    true ->
					{{FNum, V}, R} =
					    protobuffs:decode_packed(Bytes,
								     Type),
					{V, R};
				    false ->
					{{FNum, V}, R} =
					    protobuffs:decode(Bytes, Type),
					{unpack_value(V, Type), R}
				  end
			    end,
	  case lists:member(repeated, Opts) of
	    true ->
		case lists:keytake(FNum, 1, Acc) of
		  {value, {FNum, Name, List}, Acc1} ->
		      decode(Rest1, Types,
			     [{FNum, Name, [int_to_enum(Type, Value1) | List]}
			      | Acc1]);
		  false ->
		      decode(Rest1, Types,
			     [{FNum, Name, [int_to_enum(Type, Value1)]} | Acc])
		end;
	    false ->
		decode(Rest1, Types,
		       [{FNum, Name, int_to_enum(Type, Value1)} | Acc])
	  end;
      false ->
	  case lists:keyfind('$extensions', 2, Acc) of
	    {_, _, Dict} ->
		{{FNum, _V}, R} = protobuffs:decode(Bytes, bytes),
		Diff = size(Bytes) - size(R),
		<<V:Diff/binary, _/binary>> = Bytes,
		NewDict = dict:store(FNum, V, Dict),
		NewAcc = lists:keyreplace('$extensions', 2, Acc,
					  {false, '$extensions', NewDict}),
		decode(R, Types, NewAcc);
	    _ ->
		{ok, Skipped} = protobuffs:skip_next_field(Bytes),
		decode(Skipped, Types, Acc)
	  end
    end.

reverse_repeated_fields(FieldList, Types) ->
    [begin
       case lists:keyfind(FNum, 1, Types) of
	 {FNum, Name, _Type, Opts} ->
	     case lists:member(repeated, Opts) of
	       true -> {FNum, Name, lists:reverse(Value)};
	       _ -> Field
	     end;
	 _ -> Field
       end
     end
     || {FNum, Name, Value} = Field <- FieldList].

unpack_value(Binary, string) when is_binary(Binary) ->
    binary_to_list(Binary);
unpack_value(Value, _) -> Value.

to_record(driverhdr, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       driverhdr),
						   Record, Name, Val)
			  end,
			  #driverhdr{}, DecodedTuples),
    Record1;
to_record(drivermsg, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       drivermsg),
						   Record, Name, Val)
			  end,
			  #drivermsg{}, DecodedTuples),
    decode_extensions(Record1);
to_record(motorsspeed, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       motorsspeed),
						   Record, Name, Val)
			  end,
			  #motorsspeed{}, DecodedTuples),
    Record1.

decode_extensions(#drivermsg{'$extensions' =
				 Extensions} =
		      Record) ->
    Types = [{22, currentSpeed, motorsspeed, [is_record]},
	     {21, currentSpeedRequest, bool, []},
	     {20, motorsCommand, motorsspeed, [is_record]},
	     {5, listenerNum, uint32, []}, {4, ackNum, uint32, []},
	     {3, synNum, uint32, []},
	     {2, type, drivermsg_msgtype, []}],
    NewExtensions = decode_extensions(Types,
				      dict:to_list(Extensions), []),
    Record#drivermsg{'$extensions' = NewExtensions};
decode_extensions(Record) -> Record.

decode_extensions(_Types, [], Acc) ->
    dict:from_list(Acc);
decode_extensions(Types, [{Fnum, Bytes} | Tail], Acc) ->
    NewAcc = case lists:keyfind(Fnum, 1, Types) of
	       {Fnum, Name, Type, Opts} ->
		   {Value1, Rest1} = case lists:member(is_record, Opts) of
				       true ->
					   {{FNum, V}, R} =
					       protobuffs:decode(Bytes, bytes),
					   RecVal = decode(Type, V),
					   {RecVal, R};
				       false ->
					   case lists:member(repeated_packed,
							     Opts)
					       of
					     true ->
						 {{FNum, V}, R} =
						     protobuffs:decode_packed(Bytes,
									      Type),
						 {V, R};
					     false ->
						 {{FNum, V}, R} =
						     protobuffs:decode(Bytes,
								       Type),
						 {unpack_value(V, Type), R}
					   end
				     end,
		   case lists:member(repeated, Opts) of
		     true ->
			 case lists:keytake(FNum, 1, Acc) of
			   {value, {FNum, Name, List}, Acc1} ->
			       decode(Rest1, Types,
				      [{FNum, Name,
					lists:reverse([int_to_enum(Type, Value1)
						       | lists:reverse(List)])}
				       | Acc1]);
			   false ->
			       decode(Rest1, Types,
				      [{FNum, Name, [int_to_enum(Type, Value1)]}
				       | Acc])
			 end;
		     false ->
			 [{Fnum,
			   {optional, int_to_enum(Type, Value1), Type, Opts}}
			  | Acc]
		   end;
	       false -> [{Fnum, Bytes} | Acc]
	     end,
    decode_extensions(Types, Tail, NewAcc).

set_record_field(Fields, Record, '$extensions',
		 Value) ->
    Decodable = [],
    NewValue = decode_extensions(element(1, Record),
				 Decodable, dict:to_list(Value)),
    Index = list_index('$extensions', Fields),
    erlang:setelement(Index + 1, Record, NewValue);
set_record_field(Fields, Record, Field, Value) ->
    Index = list_index(Field, Fields),
    erlang:setelement(Index + 1, Record, Value).

list_index(Target, List) -> list_index(Target, List, 1).

list_index(Target, [Target | _], Index) -> Index;
list_index(Target, [_ | Tail], Index) ->
    list_index(Target, Tail, Index + 1);
list_index(_, [], _) -> -1.

extension_size(#drivermsg{'$extensions' =
			      Extensions}) ->
    dict:size(Extensions);
extension_size(_) -> 0.

has_extension(#drivermsg{'$extensions' = Extensions},
	      5) ->
    dict:is_key(5, Extensions);
has_extension(#drivermsg{'$extensions' = Extensions},
	      listenernum) ->
    dict:is_key(listenernum, Extensions);
has_extension(#drivermsg{'$extensions' = Extensions},
	      4) ->
    dict:is_key(4, Extensions);
has_extension(#drivermsg{'$extensions' = Extensions},
	      acknum) ->
    dict:is_key(acknum, Extensions);
has_extension(#drivermsg{'$extensions' = Extensions},
	      3) ->
    dict:is_key(3, Extensions);
has_extension(#drivermsg{'$extensions' = Extensions},
	      synnum) ->
    dict:is_key(synnum, Extensions);
has_extension(#drivermsg{'$extensions' = Extensions},
	      2) ->
    dict:is_key(2, Extensions);
has_extension(#drivermsg{'$extensions' = Extensions},
	      type) ->
    dict:is_key(type, Extensions);
has_extension(#drivermsg{'$extensions' = Extensions},
	      22) ->
    dict:is_key(22, Extensions);
has_extension(#drivermsg{'$extensions' = Extensions},
	      currentspeed) ->
    dict:is_key(currentspeed, Extensions);
has_extension(#drivermsg{'$extensions' = Extensions},
	      21) ->
    dict:is_key(21, Extensions);
has_extension(#drivermsg{'$extensions' = Extensions},
	      currentspeedrequest) ->
    dict:is_key(currentspeedrequest, Extensions);
has_extension(#drivermsg{'$extensions' = Extensions},
	      20) ->
    dict:is_key(20, Extensions);
has_extension(#drivermsg{'$extensions' = Extensions},
	      motorscommand) ->
    dict:is_key(motorscommand, Extensions);
has_extension(_Record, _FieldName) -> false.

get_extension(Record, motorscommand)
    when is_record(Record, drivermsg) ->
    get_extension(Record, 20);
get_extension(Record, currentspeedrequest)
    when is_record(Record, drivermsg) ->
    get_extension(Record, 21);
get_extension(Record, currentspeed)
    when is_record(Record, drivermsg) ->
    get_extension(Record, 22);
get_extension(Record, type)
    when is_record(Record, drivermsg) ->
    get_extension(Record, 2);
get_extension(Record, synnum)
    when is_record(Record, drivermsg) ->
    get_extension(Record, 3);
get_extension(Record, acknum)
    when is_record(Record, drivermsg) ->
    get_extension(Record, 4);
get_extension(Record, listenernum)
    when is_record(Record, drivermsg) ->
    get_extension(Record, 5);
get_extension(#drivermsg{'$extensions' = Extensions},
	      Int)
    when is_integer(Int) ->
    case dict:find(Int, Extensions) of
      {ok, {_Rule, Value, _Type, _Opts}} -> {ok, Value};
      {ok, Binary} -> {raw, Binary};
      error -> undefined
    end;
get_extension(_Record, _FieldName) -> undefined.

set_extension(#drivermsg{'$extensions' = Extensions} =
		  Record,
	      listenernum, Value) ->
    NewExtends = dict:store(5,
			    {optional, Value, uint32, none}, Extensions),
    {ok, Record#drivermsg{'$extensions' = NewExtends}};
set_extension(#drivermsg{'$extensions' = Extensions} =
		  Record,
	      acknum, Value) ->
    NewExtends = dict:store(4,
			    {optional, Value, uint32, none}, Extensions),
    {ok, Record#drivermsg{'$extensions' = NewExtends}};
set_extension(#drivermsg{'$extensions' = Extensions} =
		  Record,
	      synnum, Value) ->
    NewExtends = dict:store(3,
			    {optional, Value, uint32, none}, Extensions),
    {ok, Record#drivermsg{'$extensions' = NewExtends}};
set_extension(#drivermsg{'$extensions' = Extensions} =
		  Record,
	      type, Value) ->
    NewExtends = dict:store(2,
			    {required, Value, drivermsg_msgtype, none},
			    Extensions),
    {ok, Record#drivermsg{'$extensions' = NewExtends}};
set_extension(#drivermsg{'$extensions' = Extensions} =
		  Record,
	      currentspeed, Value) ->
    NewExtends = dict:store(22,
			    {optional, Value, motorsspeed, none}, Extensions),
    {ok, Record#drivermsg{'$extensions' = NewExtends}};
set_extension(#drivermsg{'$extensions' = Extensions} =
		  Record,
	      currentspeedrequest, Value) ->
    NewExtends = dict:store(21,
			    {optional, Value, bool, none}, Extensions),
    {ok, Record#drivermsg{'$extensions' = NewExtends}};
set_extension(#drivermsg{'$extensions' = Extensions} =
		  Record,
	      motorscommand, Value) ->
    NewExtends = dict:store(20,
			    {optional, Value, motorsspeed, none}, Extensions),
    {ok, Record#drivermsg{'$extensions' = NewExtends}};
set_extension(Record, _, _) -> {error, Record}.

