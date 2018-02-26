-module(db).
-author("Daniel").

%% API
-export([new/0, destroy/1, write/3, read/2, delete/2, match/2, match/3]).

%Daniel Gilljam 

%empty{root node. data strutured as {Key, Value, left, right}}
new() ->
  {'empty'}.
destroy(_) ->
  ok.

%Base case of insert, that is if it's an empty node. The function returns a new tree
write(Key, Val, {'empty'}) ->
  {Key, Val, {'empty'}, {'empty'}};
%recusivly go left if NewKey is smaller than the current Key
write(NewKey, NewVal, {Key, Val, LeftBranch, RightBranch}) when NewKey < Key ->
  {Key, Val, write(NewKey, NewVal, LeftBranch), RightBranch};

%recusivly go right if NewKey is smaller than the current Key
write(NewKey, NewVal, {Key, Val, LeftBranch, RightBranch}) when NewKey > Key ->
  {Key, Val, LeftBranch, write(NewKey, NewVal, RightBranch)};

% if the Key is already in the data structure, just return the same data structure
write(Key, Val, {Key, _, LeftBranch, RightBranch}) ->
  {Key, Val, LeftBranch, RightBranch}.


% If no return error
delete(_, {'empty'}) ->
  {error, invalid};
delete(_, {_, _, {'empty'}, {'empty'}}) ->
  {'empty'};

%return the left node
delete(_, {_, _, LeftBranch, {'empty'}}) ->
  LeftBranch;

%return the right node
delete(_, {_, _, {'empty'}, RightBranch}) ->
  RightBranch;
% Recursively go left if the Key to the deleted is less than the current key
delete(DelKey, {Key, Value, LeftBranch, RightBranch}) when DelKey < Key ->
  {Key, Value, delete(DelKey, LeftBranch), RightBranch};
  % Recursively go right if the Key to the deleted is less than the current key
delete(DelKey, {Key, Value, LeftBranch, RightBranch}) when DelKey > Key ->
  {Key, Value, LeftBranch, delete(DelKey, RightBranch)};
%When the key is found, rebuild the tree without the node that contained the Key
delete(Key, {Key, _, LeftBranch, RightBranch}) ->
  {Key2, Value2, LeftBranch2, _RightBranch} = rebuild(LeftBranch),
  {Key2, Value2, LeftBranch2, RightBranch}.

%Rebuilding when a node is deleted
rebuild({Key, Value, {'empty'}, {'empty'}}) ->
  {Key, Value, {'empty'}, {'empty'}};
rebuild({Key, Value, LeftBranch, {'empty'}}) ->
  {Key, Value, LeftBranch, {'empty'}};
rebuild({Key, Value, LeftBranch, RightBranch}) ->
  {Key2, Value2, LeftBranch2, RightBranch2} = rebuild(RightBranch),
  {Key2, Value2, {Key, Value, LeftBranch, RightBranch2}, 
  {Key, Value, LeftBranch2, {'empty'}}}.

%If not found, return eror
read(_, {'empty'}) ->
  {error, invalid};
%Returning the value of  thefound Key
read(Key, {Key, Val, _, _}) ->
  {ok, Val};
%Recursively go left in the tree to try and find the Key if the Key is less than current Key
read(Key, {NodeKey, _, LeftBranch, _}) when Key < NodeKey ->
  read(Key, LeftBranch);
%Otherwise recursivley go right to find the matching Key
read(Key, {_, _, _, RightBranch}) ->
  read(Key, RightBranch).

%%helper method to save all value results matches
match(Value, Db) -> {ok,  match(Value, Db, [])}.

match(Value, {Key, Value, LeftBranch, RightBranch}, Result) ->
  [Key | match(Value, LeftBranch, match(Value, RightBranch, Result))];
match(Value, {_, _, LeftBranch, RightBranch}, Result) ->
  match(Value, RightBranch, match(Value, LeftBranch, Result));
match(_, {'empty'}, Result) ->
  Result.
