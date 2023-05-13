from fastapi import FastAPI
from pyswip import Prolog

prolog = Prolog()
prolog.consult("wordle-sessioned.pl")

app = FastAPI()

@app.get("/")
def read_root():
    return {"Hello": "World"}