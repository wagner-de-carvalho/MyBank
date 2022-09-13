-module(mybank).

-export([start/0, stop/0]).
-export([deposit/2]).
-export([balance/1]).
-export([withdraw/2]).

start() ->
    mybank_sup:start().

stop() ->
    mybank_sup:stop().

deposit(AccountId, Amount) ->
    mybank_atm:deposit(AccountId, Amount).

balance(AccountId) ->
    mybank_atm:balance(AccountId).

withdraw(AccountId, Amount) ->
    mybank_atm:withdraw(AccountId, Amount).
