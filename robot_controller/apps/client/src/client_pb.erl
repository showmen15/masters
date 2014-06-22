-file("src/client_pb.erl", 1).

-module(client_pb).

-export([encode_robotcommand/1, decode_robotcommand/1,
	 delimited_decode_robotcommand/1,
	 encode_commandmessage/1, decode_commandmessage/1,
	 delimited_decode_commandmessage/1, encode_ack/1,
	 decode_ack/1, delimited_decode_ack/1,
	 encode_setupmessage/1, decode_setupmessage/1,
	 delimited_decode_setupmessage/1,
	 encode_robotfullstate/1, decode_robotfullstate/1,
	 delimited_decode_robotfullstate/1,
	 encode_statemessage/1, decode_statemessage/1,
	 delimited_decode_statemessage/1]).

-export([has_extension/2, extension_size/1,
	 get_extension/2, set_extension/3]).

-export([decode_extensions/1]).

-export([encode/1, decode/2, delimited_decode/2]).

-record(robotcommand,
	{frontleft, frontright, rearleft, rearright,
	 fearfactor}).

-record(commandmessage, {type, robotcommand}).

-record(ack, {}).

-record(setupmessage, {robotname}).

-record(robotfullstate,
	{robotname, x, y, theta, timestamp, fearfactor}).

-record(statemessage, {robotstate, event}).

encode([]) -> [];
encode(Records) when is_list(Records) ->
    delimited_encode(Records);
encode(Record) -> encode(element(1, Record), Record).

encode_robotcommand(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_robotcommand(Record)
    when is_record(Record, robotcommand) ->
    encode(robotcommand, Record).

encode_commandmessage(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_commandmessage(Record)
    when is_record(Record, commandmessage) ->
    encode(commandmessage, Record).

encode_ack(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_ack(Record) when is_record(Record, ack) ->
    encode(ack, Record).

encode_setupmessage(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_setupmessage(Record)
    when is_record(Record, setupmessage) ->
    encode(setupmessage, Record).

encode_robotfullstate(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_robotfullstate(Record)
    when is_record(Record, robotfullstate) ->
    encode(robotfullstate, Record).

encode_statemessage(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_statemessage(Record)
    when is_record(Record, statemessage) ->
    encode(statemessage, Record).

encode(statemessage, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(statemessage, Record) ->
    [iolist(statemessage, Record)
     | encode_extensions(Record)];
encode(robotfullstate, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(robotfullstate, Record) ->
    [iolist(robotfullstate, Record)
     | encode_extensions(Record)];
encode(setupmessage, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(setupmessage, Record) ->
    [iolist(setupmessage, Record)
     | encode_extensions(Record)];
encode(ack, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(ack, Record) ->
    [iolist(ack, Record) | encode_extensions(Record)];
encode(commandmessage, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(commandmessage, Record) ->
    [iolist(commandmessage, Record)
     | encode_extensions(Record)];
encode(robotcommand, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(robotcommand, Record) ->
    [iolist(robotcommand, Record)
     | encode_extensions(Record)].

encode_extensions(_) -> [].

delimited_encode(Records) ->
    lists:map(fun (Record) ->
		      IoRec = encode(Record),
		      Size = iolist_size(IoRec),
		      [protobuffs:encode_varint(Size), IoRec]
	      end,
	      Records).

iolist(statemessage, Record) ->
    [pack(1, repeated,
	  with_default(Record#statemessage.robotstate, none),
	  robotfullstate, []),
     pack(2, optional,
	  with_default(Record#statemessage.event, none),
	  statemessage_event, [])];
iolist(robotfullstate, Record) ->
    [pack(1, required,
	  with_default(Record#robotfullstate.robotname, none),
	  string, []),
     pack(2, required,
	  with_default(Record#robotfullstate.x, none), double,
	  []),
     pack(3, required,
	  with_default(Record#robotfullstate.y, none), double,
	  []),
     pack(4, required,
	  with_default(Record#robotfullstate.theta, none), double,
	  []),
     pack(5, required,
	  with_default(Record#robotfullstate.timestamp, none),
	  int64, []),
     pack(6, required,
	  with_default(Record#robotfullstate.fearfactor, none),
	  double, [])];
iolist(setupmessage, Record) ->
    [pack(1, required,
	  with_default(Record#setupmessage.robotname, none),
	  string, [])];
iolist(ack, _Record) -> [];
iolist(commandmessage, Record) ->
    [pack(1, required,
	  with_default(Record#commandmessage.type, none),
	  commandmessage_type, []),
     pack(2, optional,
	  with_default(Record#commandmessage.robotcommand, none),
	  robotcommand, [])];
iolist(robotcommand, Record) ->
    [pack(1, required,
	  with_default(Record#robotcommand.frontleft, none),
	  double, []),
     pack(2, required,
	  with_default(Record#robotcommand.frontright, none),
	  double, []),
     pack(3, required,
	  with_default(Record#robotcommand.rearleft, none),
	  double, []),
     pack(4, required,
	  with_default(Record#robotcommand.rearright, none),
	  double, []),
     pack(5, required,
	  with_default(Record#robotcommand.fearfactor, none),
	  double, [])].

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

enum_to_int(commandmessage_type, 'ROBOT_COMMAND') -> 2;
enum_to_int(commandmessage_type, 'REQUEST_STATE') -> 1;
enum_to_int(statemessage_event, 'RESET') -> 3;
enum_to_int(statemessage_event, 'START') -> 2;
enum_to_int(statemessage_event, 'STOP') -> 1.

int_to_enum(commandmessage_type, 2) -> 'ROBOT_COMMAND';
int_to_enum(commandmessage_type, 1) -> 'REQUEST_STATE';
int_to_enum(statemessage_event, 3) -> 'RESET';
int_to_enum(statemessage_event, 2) -> 'START';
int_to_enum(statemessage_event, 1) -> 'STOP';
int_to_enum(_, Val) -> Val.

decode_robotcommand(Bytes) when is_binary(Bytes) ->
    decode(robotcommand, Bytes).

decode_commandmessage(Bytes) when is_binary(Bytes) ->
    decode(commandmessage, Bytes).

decode_ack(Bytes) when is_binary(Bytes) ->
    decode(ack, Bytes).

decode_setupmessage(Bytes) when is_binary(Bytes) ->
    decode(setupmessage, Bytes).

decode_robotfullstate(Bytes) when is_binary(Bytes) ->
    decode(robotfullstate, Bytes).

decode_statemessage(Bytes) when is_binary(Bytes) ->
    decode(statemessage, Bytes).

delimited_decode_statemessage(Bytes) ->
    delimited_decode(statemessage, Bytes).

delimited_decode_robotfullstate(Bytes) ->
    delimited_decode(robotfullstate, Bytes).

delimited_decode_setupmessage(Bytes) ->
    delimited_decode(setupmessage, Bytes).

delimited_decode_ack(Bytes) ->
    delimited_decode(ack, Bytes).

delimited_decode_commandmessage(Bytes) ->
    delimited_decode(commandmessage, Bytes).

delimited_decode_robotcommand(Bytes) ->
    delimited_decode(robotcommand, Bytes).

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
decode(statemessage, Bytes) when is_binary(Bytes) ->
    Types = [{2, event, statemessage_event, []},
	     {1, robotstate, robotfullstate, [is_record, repeated]}],
    Defaults = [{1, robotstate, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(statemessage, Decoded);
decode(robotfullstate, Bytes) when is_binary(Bytes) ->
    Types = [{6, fearfactor, double, []},
	     {5, timestamp, int64, []}, {4, theta, double, []},
	     {3, y, double, []}, {2, x, double, []},
	     {1, robotname, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(robotfullstate, Decoded);
decode(setupmessage, Bytes) when is_binary(Bytes) ->
    Types = [{1, robotname, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(setupmessage, Decoded);
decode(ack, Bytes) when is_binary(Bytes) ->
    Types = [],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(ack, Decoded);
decode(commandmessage, Bytes) when is_binary(Bytes) ->
    Types = [{2, robotcommand, robotcommand, [is_record]},
	     {1, type, commandmessage_type, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(commandmessage, Decoded);
decode(robotcommand, Bytes) when is_binary(Bytes) ->
    Types = [{5, fearfactor, double, []},
	     {4, rearright, double, []}, {3, rearleft, double, []},
	     {2, frontright, double, []},
	     {1, frontleft, double, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(robotcommand, Decoded).

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

to_record(statemessage, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       statemessage),
						   Record, Name, Val)
			  end,
			  #statemessage{}, DecodedTuples),
    Record1;
to_record(robotfullstate, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       robotfullstate),
						   Record, Name, Val)
			  end,
			  #robotfullstate{}, DecodedTuples),
    Record1;
to_record(setupmessage, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       setupmessage),
						   Record, Name, Val)
			  end,
			  #setupmessage{}, DecodedTuples),
    Record1;
to_record(ack, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields, ack),
						   Record, Name, Val)
			  end,
			  #ack{}, DecodedTuples),
    Record1;
to_record(commandmessage, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       commandmessage),
						   Record, Name, Val)
			  end,
			  #commandmessage{}, DecodedTuples),
    Record1;
to_record(robotcommand, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       robotcommand),
						   Record, Name, Val)
			  end,
			  #robotcommand{}, DecodedTuples),
    Record1.

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

extension_size(_) -> 0.

has_extension(_Record, _FieldName) -> false.

get_extension(_Record, _FieldName) -> undefined.

set_extension(Record, _, _) -> {error, Record}.

