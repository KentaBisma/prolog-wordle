from pyswip import Prolog

prolog = Prolog()
prolog.consult("wordle-sessioned.pl")
[*prolog.query("wordle")]
print(*prolog.query("guess(kalor, R)"))
print(*prolog.query("guess(jubah, R)"))
print(*prolog.query("guess(cipta, R)"))
print(*prolog.query("guess(zakar, R)"))
print(*prolog.query("guess(afiks, R)"))
print(*prolog.query("has_session(X, Y)"))
print(*prolog.query("guess(ketan, R)"))