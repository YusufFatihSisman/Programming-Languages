flight(canakkale, erzincan, 6).
flight(erzincan, canakkale, 6).
flight(erzincan, antalya, 3).
flight(antalya, erzincan, 3).
flight(antalya, izmir, 2).
flight(antalya, diyarbakir, 4).
flight(izmir, antalya, 2).
flight(izmir, istanbul, 2).
flight(izmir, ankara, 6).
flight(diyarbakir, antalya, 4).
flight(diyarbakir, ankara, 8).
flight(istanbul, izmir, 2).
flight(istanbul, ankara, 1).
flight(istanbul, rize, 4).
flight(ankara, istanbul, 1).
flight(ankara, rize, 5).
flight(ankara, izmir, 6).
flight(ankara, diyarbakir, 8).
flight(ankara, van, 4).
flight(rize, istanbul, 4).
flight(rize, ankara, 5).
flight(van, ankara, 4).
flight(van, gaziantep, 3).
flight(gaziantep, van, 3).

routeHelper(X, Y, C, A) :-
    flight(X, Y, C),
    not(member(Y, A)).

routeHelper(X, Y, C, A) :-
    flight(X, Z, D),
    not(member(Z, A)),
    routeHelper(Z, Y, B, [X|A]),
    C is B + D.

route(X, Y, C) :-
    routeHelper(X, Y, C, []).
