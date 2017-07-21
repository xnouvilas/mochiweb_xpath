%% xpath_functions.erl
%% @author Pablo Polvorin
%% @doc Some core xpath functions that can be used in xpath expressions
%% created on 2008-05-07
-module(mochiweb_xpath_functions).

-export([lookup_function/1]).


%% Default functions.
%% The format is: {FunctionName, fun(), FunctionSignature}
%% WildCard argspec must be the last spec in list.
%%
%% @type FunctionName = atom()
%% @type FunctionSignature = [XPathArgSpec]
%% @type XPathArgSpec = XPathType | WildCardArgSpec
%% @type WildCardArgSpec = {'*', XPathType}
%% @type XPathType = node_set | string | number | boolean
%%
%% The engine is responsable of calling the function with
%% the correct arguments, given the function signature.
-spec lookup_function(atom()) -> mochiweb_xpath:xpath_fun_spec() | false.
lookup_function('last') ->
    {'last',fun last/2,[]};
lookup_function('position') ->
    {'position',fun position/2,[]};
lookup_function('count') ->
    {'count',fun count/2,[node_set]};
lookup_function('concat') ->
    {'concat',fun concat/2,[{'*', string}]};
lookup_function('concat-list') ->
    {'concat-list',fun 'concat-list'/2,[string,node_set,string]};
lookup_function('name') ->
    {'name',fun 'name'/2,[node_set]};
lookup_function('starts-with') ->
    {'starts-with', fun 'starts-with'/2,[string,string]};
lookup_function('ends-with') ->
    {'ends-with', fun 'ends-with'/2,[string,string]};
lookup_function('contains') ->
    {'contains', fun 'contains'/2,[string,string]};
lookup_function('between') ->
    {'between', fun 'between'/2,[string,number,number]};
lookup_function('substring') ->
    {'substring', fun substring/2,[string,number,number]};
lookup_function('substring-list') ->
    {'substring-list', fun 'substring-list'/2,[node_set,number,number]};
lookup_function('replace') ->
    {'replace', fun replace/2,[string,string,string]};
lookup_function('replace-list') ->
    {'replace-list', fun 'replace-list'/2,[node_set,string,string]};
lookup_function('sum') ->
    {'sum', fun sum/2,[node_set]};
lookup_function('string-length') ->
    {'string-length', fun 'string-length'/2,[string]};

lookup_function('regex-match') ->
    {'regex-match', fun 'regex-match'/2,[string,string]};
lookup_function('regex-take') ->
    {'regex-take', fun 'regex-take'/2,[string,string]};
lookup_function('regex-replace') ->
    {'regex-replace', fun 'regex-replace'/2,[string,string,string]};
lookup_function('regex-replace-list') ->
    {'regex-replace-list', fun 'regex-replace-list'/2,[node_set,string,string]};

lookup_function('split') ->
    {'split', fun split/2,[string,string]};
lookup_function('join') ->
    {'join', fun join/2,[node_set,string]};
lookup_function('join-each') ->
    {'join-each', fun 'join-each'/2,[node_set,string]};
lookup_function('take') ->
    {'take', fun take/2,[node_set,number]};
lookup_function('take-each') ->
    {'take-each', fun 'take-each'/2,[node_set,number]};
lookup_function('take-rows') ->
    {'take-rows', fun 'take-rows'/2,[node_set,number,number]};

lookup_function('if-else') ->
    {'if-else', fun 'if-else'/2,[string,string,string]};
lookup_function('if-else-list') ->
    {'if-else-list', fun 'if-else-list'/2,[string,node_set,node_set]};

lookup_function('string') ->
    {'string', fun 'string'/2, [node_set]};
lookup_function('string-list') ->
    {'string-list', fun 'string-list'/2, [node_set]};
lookup_function('not') ->
    {'not', fun x_not/2, [boolean]};
lookup_function(_) ->
    false.

%% @doc Function: int last()
%%      The last function returns the context size of the current node
last({ctx, _, _, _, _, Size} = _Ctx, []) ->
    Size.

%% @doc Function: number position()
%%      The position function returns the position of the current node
position({ctx, _, _, _, Position, _} = _Ctx, []) ->
    Position.

%% @doc Function: number count(node-set)
%%      The count function returns the number of nodes in the
%%      argument node-set.
count(_Ctx,[NodeList]) ->
    length(NodeList).

%% @doc Function: concat(binary, binary, ...)
%%      Concatenate string arguments (variable length)
concat(_Ctx, BinariesList) ->
    %% list_to_binary()
    << <<Str/binary>> || Str <- BinariesList>>.

%% @doc Function: concat(binary, NodeList, binary)
%%      Concatenate prefixes and suffixes to string for each entry in list
'concat-list'(_Ctx, [Prefix,NodeList,Suffix]) ->
    lists:map(fun(Node) -> <<Prefix/binary,Node/binary,Suffix/binary>> end, NodeList).

%% @doc Function: string name(node-set?)
'name'(_Ctx,[[{Tag,_,_,_}|_]]) ->
    Tag.

%% @doc Function: boolean starts-with(string, string)
%%      The starts-with function returns true if the first argument string
%%      starts with the second argument string, and otherwise returns false.
'starts-with'(_Ctx,[Left,Right]) ->
    Size = size(Right),
    case Left of
        <<Right:Size/binary,_/binary>> -> true;
        _ -> false
    end.

%% @doc Function: boolean ends-with(string, string)
%%      The ends-with function returns true if the first argument string
%%      ends with the second argument string, and otherwise returns false.
'ends-with'(_Ctx,[Left,Right]) ->
    Size = size(Right),
    Dsize = size(Left) - Size,
    case Left of
        <<_:Dsize/binary,Right:Size/binary>> -> true;
        _ -> false
    end.

%% @doc Function: checks that Where contains What
contains(_Ctx,[Where, What]) ->
    case binary:match(Where, [What]) of
        nomatch ->
            false;
        {_, _} ->
            true
    end.

%% @doc Function: checks that Where is between two integers
between(_Ctx,[What,Low,High]) ->
    case is_integer(What) of
        true ->
            Number = What;
        false ->
            {Number, _} = string:to_integer(binary_to_list(What))
    end,
    (Number >= Low) and (Number =< High).

%% @doc Function: string substring(string, number, number?)
%%      The substring function returns the substring of the first argument
%%      starting at the position specified in the second argument with length
%%      specified in the third argument
substring(_Ctx,[<<>>,_Start,_Length]) -> <<>>;
substring(_Ctx,[String,Start,Length]) when is_binary(String)->

    Size = size(String),

    case Start < 1 of
        false ->
            Before = Start - 1;
        true ->
            Before = Size + Start
    end,

    Max = Size - Before,
    case (Start + Length) =< size(String) + 1 of
        true ->
            After = Size - Before - Length,
            <<_:Before/binary,R:Length/binary,_:After/binary>> = String,
            R;
        false ->
            After = 0,
            <<_:Before/binary,R:Max/binary,_:After/binary>> = String,
            R
    end.

%% @doc Function: string substring-list(node-set, number, number?)
%%      The substring function returns the substring of the first argument
%%      starting at the position specified in the second argument with length
%%      specified in the third argument for every node in list
'substring-list'(Ctx,[NodeList,Start,Length]) ->
    lists:map(fun(Node) -> substring(Ctx,[Node,Start,Length]) end, NodeList).


%% @doc Function: replace(binary, binary, binary)
%%      Replaces needle in haystack with replace
replace(_Ctx,[Haystack,Needle,Replace]) ->
    CleanHaystack = binary:replace(Haystack, <<"\n">>, <<" ">>, [global]),
    binary:replace(CleanHaystack,Needle,Replace,[global]).


%% @doc Function: replace-list(NodeList, binary, binary)
%%      Replace needle in haystack with replace for each entry in list
'replace-list'(Ctx, [NodeList,Needle,Replace]) ->
    lists:map(fun(Haystack) ->
        case is_binary(Haystack) of
            true ->
                replace(Ctx, [Haystack,Needle,Replace]);
            false ->
                BinaryHaystack = join(Ctx,[lists:flatten(Haystack), <<" ">>]),
                replace(Ctx, [BinaryHaystack,Needle,Replace])
        end

    end, NodeList).


%% @doc Function: number sum(node-set)
%%      The sum function returns the sum, for each node in the argument
%%      node-set, of the result of converting the string-values of the node
%%      to a number.
sum(_Ctx,[Values]) ->
    lists:sum([mochiweb_xpath_utils:number_value(V) || V <- Values]).

%% @doc Function: number string-length(string?)
%%      The string-length returns the number of characters in the string
%%      TODO: this isn't true: currently it returns the number of bytes
%%            in the string, that isn't the same
'string-length'(_Ctx,[String]) ->
    size(String).

%% @doc Function: regex-match split(string, string, string)
%%      Replace string content by regular expression
'regex-match'(_Ctx,[<<>>,_Match]) -> [];
'regex-match'(_Ctx,[String,Match]) ->
    case re:run(String, Match, [global, {capture, all, binary}]) of
        {match,Results} ->
            Results;
        nomatch ->
            Results = []
    end,
    lists:flatten(Results).

%% @doc Function: regex-replace split(string, string)
%%      Replace contents of a string by regular expression
'regex-replace'(_Ctx,[<<>>,_Match,_Replace]) -> <<>>;
'regex-replace'(_Ctx,[String,Match,Replace]) ->
    re:replace(String, Match, Replace, [global, {return, list}]).


%% @doc Function: regex-take split(string, string)
%%      Return first regex match
'regex-take'(_Ctx,[<<>>,_Match]) -> <<>>;
'regex-take'(_Ctx,[String,Match]) ->
    case re:run(String,Match,[global,{capture,[1],binary}]) of
      nomatch ->
        <<>>;
      {match, [[Result]]} ->
        Result
    end.


%% @doc Function: regex-replace-list split(string, string, string)
%%      Replace string content by regular expression for every entry in list
'regex-replace-list'(_Ctx,[[],_Match,_Replace]) -> [];
'regex-replace-list'(Ctx,[NodeList,Match,Replace]) when is_list(NodeList) ->
    lists:map(fun(Node) -> 'regex-replace'(Ctx,[Node,Match,Replace]) end, NodeList).

%% @doc Function: nodes-set split(string, string)
%%      Split a string into nodes
split(_Ctx,[<<>>,_Separator]) -> [];
split(_Ctx,[String,Separator]) ->
    PreparedString = re:replace(String, Separator, <<"¦">>, [global,{return,list}]),
    Pieces = string:tokens(PreparedString, "¦"),
    lists:map(fun(Piece) -> list_to_binary(Piece) end, Pieces).

%% @doc Function: string join(node-set, string)
%%      Split a string into nodes
join(_Ctx,[NodeList,Glue]) ->
    StringList = lists:map(fun(Node) -> binary_to_list(normalize(Node)) end, NodeList),
    list_to_binary(string:join(StringList,binary_to_list(Glue))).

normalize(Value) when is_binary(Value) ->
  Value;
normalize(Value) ->
  [Head|_] = Value,
  Head.

%% @doc Function: string join(node-set, string)
%%      Split a string into nodes
'join-each'(_Ctx,[NodeList,Glue]) ->
  lists:map(fun(Node) ->
    List = normalize_list(Node),
    StringList = lists:map(fun(Entry) -> binary_to_list(Entry) end, List),
    list_to_binary(string:join(StringList,binary_to_list(Glue)))
  end, NodeList).

normalize_list({_Elem, _Attr, Elements,_Pos}) ->
  lists:flatten(lists:map(fun(Element) -> normalize_list(Element) end, Elements));
normalize_list(Value) when is_binary(Value) ->
  clean(Value);
normalize_list(Elements) ->
  lists:map(fun(Element) -> normalize_list(Element) end, Elements).

%% @doc Function: node-set clean(string)
%%      Clean string removing html entities, line-breaks, etc.
clean(Binary) ->
  String = re:replace(Binary, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
  list_to_binary(String).

%% @doc Function: node-set take(node-set, number)
%%      Split a string into nodes
take(_Ctx,[NodeList,-1]) ->
    ListSize = length(NodeList),
    lists:nth(ListSize, NodeList);
take(_Ctx,[NodeList,0]) ->
    lists:nth(1, NodeList);
take(_Ctx,[NodeList,Index]) ->
    ListSize = length(NodeList),
    case Index =< ListSize of
        true ->
            lists:nth(Index, NodeList);
        false ->
            <<>>
    end.

%% @doc Function: node-set take(node-set, index)
%%      Selects element index from lists inside a list
'take-each'(Ctx,[NodeList,Index]) ->
    lists:map(fun(Node) -> take(Ctx,[Node,Index]) end, NodeList).

%% @doc Function: node-set take(node-set, columns, rows)
%%      Selects full rows in tables
'take-rows'(_Ctx,[NodeList,Columns,Rows]) ->
    lists:map(fun(Row) -> lists:sublist(NodeList,(Row*Columns - Columns),Row*Columns) end, lists:seq(1,Rows)).

%% @doc Function: node-set take(string, string, string)
%%      Selects first or second choices depending on condition empty or not
'if-else'(_Ctx,[<<>>,_First,Second]) ->
    Second;
'if-else'(_Ctx,[_Condition,First,_Second]) ->
    First.

%% @doc Function: node-set take(string, string, string)
%%      Selects first or second list depending on condition empty or not
'if-else-list'(_Ctx,[<<>>,_First,Second]) ->
    Second;
'if-else-list'(_Ctx,[_Condition,First,_Second]) ->
    First.

%%  @doc Function: string string(node_set)
%%
%%       The sum function returns the string representation of the
%%       nodes in a node-set. This is different from text() in that
%%       it concatenates each bit of the text in the node along with the text in
%%       any children nodes along the way, in order.
%%       Note: this differs from normal xpath in that it returns a list of strings, one
%%       for each node in the node set, as opposed to just the first node.
'string'(_Ctx, [NodeList]) ->
    lists:map(fun({_Elem, _Attr, Children,_Pos}) -> concat_child_text(Children, []) end, NodeList).

concat_child_text([], Results) ->
    List = lists:map(fun(Result) -> remove_comments(Result) end, Results),
    list_to_binary(lists:reverse(List));
concat_child_text([{_,_,Children,_} | Rest], Result) ->
    concat_child_text(Rest, [concat_child_text(Children, []) | Result]);
concat_child_text([X | Rest], Result) ->
    concat_child_text(Rest, [X | Result]).

remove_comments({comment, _Comment}) -> <<>>;
remove_comments(String) -> String.

'string-list'(_Ctx, [NodeList]) ->
    lists:map(fun({_Elem, _Attr, Children,_Pos}) -> list_child_text(Children, []) end, NodeList).

list_child_text([], Result) ->
    NodeList = lists:reverse(Result),
    lists:map(fun(Node) -> normalize_list(Node) end, NodeList);
list_child_text([{_,_,Children,_} | Rest], Result) ->
    list_child_text(Rest, [list_child_text(Children, []) | Result]);
list_child_text([X | Rest], Result) ->
    list_child_text(Rest, [X | Result]).

x_not(_Ctx, [Bool]) ->
    not Bool.

