:- dynamic exist_letter/1, green/1, yellow/1, word/1.

wordle:-
    open('words.txt',read,Str),
    read_line(Str,L_word),
    close(Str),
    random_member(Answer,L_word),

    teardown,
    gen_words(L_word),
    gen_existing_letters(Answer),

    write("Wordle started!"), nl,

    process_guess(Guess),
    game_loop(Guess, Answer, 5).

process_guess(Guess):-
    write("Guess: "), nl,
    read(Guess),
    check_exist(word(Guess)), !.

process_guess(Guess):- 
    write("That word does not exist!"), nl,
    process_guess(Guess).

gen_words([]).

gen_words([H|Tail]):-
    assertz(word(H)),
    gen_words(Tail).

gen_existing_letters(Answer):-
    atom_chars(Answer,L_answer),
    gen_existing_letters_(L_answer).

gen_existing_letters_([]):- !.

gen_existing_letters_([H|Tail]):-
    assertz(exist_letter(H)),
    gen_existing_letters_(Tail).

game_loop(_, Answer, 0):-
    teardown,
    concat("You lose! The answer is ", Answer, Str),
    write(Str), !.

game_loop(Guess, Answer, Tries):-
    atom_chars(Answer,L_answer),
    atom_chars(Guess,L_guess),
    check_word(L_guess, L_answer, [], Status),
    check_yellows(L_guess, Status, [], FinalStatus),
    write(FinalStatus), nl,
    check_status(Tries, Answer, FinalStatus).

teardown:-
    retractall(word(_)),
    retractall(exist_letter(_)),
    retractall(green(_)),
    retractall(yellow(_)).    

check_status(_, Answer, [green,green,green,green,green]):-
    teardown,
    concat("You win! The answer is ", Answer, Str),
    write(Str), !.

check_status(Tries, Answer, _):-
    retractall(green(_)),
    retractall(yellow(_)),
    process_guess(Guess),
    Tries_ is Tries-1,
    game_loop(Guess, Answer, Tries_).

read_line(Stream,[]):-
    at_end_of_stream(Stream), !.

read_line(Stream,[X|L]):-
    read(Stream,X),
    read_line(Stream,L).

check_word([], [], Status, Status).

check_word([G|Gs],[W|Ws], Acc, Status):-
    G = W,
    append(Acc,[green],NewAcc),
    assertz(green(G)),
    check_word(Gs,Ws,NewAcc,Status), !.

check_word([G|Gs],[W|Ws],Acc,Status):-
    G \= W,
    \+ check_exist(exist_letter(G)),
    append(Acc,[gray],NewAcc),
    check_word(Gs,Ws,NewAcc,Status), !.

check_word([_|Gs],[_|Ws],Acc,Status):-
    append(Acc,[noop],NewAcc),
    check_word(Gs,Ws,NewAcc,Status).

check_yellows([], [], Status, Status).

check_yellows([_|Gs], [S|Ss], Acc, FinalStatus):-
    S = green,
    append(Acc,[green],NewAcc),
    check_yellows(Gs, Ss, NewAcc, FinalStatus), !.

check_yellows([_|Gs], [S|Ss], Acc, FinalStatus):-
    S = gray,
    append(Acc,[gray],NewAcc),
    check_yellows(Gs, Ss, NewAcc, FinalStatus), !.

check_yellows([G|Gs], [S|Ss], Acc, FinalStatus):-
    S = noop,
    check_guessed(G),
    append(Acc,[yellow],NewAcc),
    assertz(yellow(G)),
    check_yellows(Gs, Ss, NewAcc, FinalStatus), !.

check_yellows([_|Gs], [_|Ss], Acc, FinalStatus):-
    append(Acc,[gray],NewAcc),
    check_yellows(Gs, Ss, NewAcc, FinalStatus), !.

check_exist(Term):-
    findall(_, Term, Bag),
    length(Bag, Length),
    Length > 0.

check_guessed(Letter):-
    findall(_, green(Letter), GreenBag),
    length(GreenBag, GreenLength),
    findall(_, yellow(Letter), YellowBag),
    length(YellowBag, YellowLength),
    findall(_, exist_letter(Letter), ExistBag),
    length(ExistBag, ExistLength),
    PartiallyGuessed is YellowLength + GreenLength,
    ExistLength > PartiallyGuessed.