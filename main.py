from fastapi import FastAPI
from fastapi.responses import HTMLResponse
from fastapi.staticfiles import StaticFiles

from pyswip import Prolog

import os

prolog = Prolog()
prolog.consult("wordle-sessioned.pl")

app = FastAPI()
app.mount("/static", StaticFiles(directory="static"), name="static")

HTML_PATH = './static/html/'

def html(path):
    content = ""
    with open(HTML_PATH + path, encoding='utf-8') as file:
        content = file.read()
    return HTMLResponse(content=content, status_code=200)

@app.get("/", response_class=HTMLResponse)
def read_root():
    [*prolog.query('wordle')]
    return html("index.html")

@app.get("/check")
def check(word):
    if has_session():
        b = len([*prolog.query(f'check_exist(word({word}))')]) != 0
        return {"wordExists": b}
    else: return {"error": "There are no valid sessions"}

@app.get("/validate")
def validate(word):
    if has_session(): return {"result": [*prolog.query(f'guess({word}, R)')][0]['R']}
    else: return {"error": "There are no valid sessions"}


@app.get("/hints")
def hints():
    if has_session():
        return {
            "validWords": [*prolog.query('get_all_probable_valid_words(R)')][0]['R'],
            "solutions": [*prolog.query('get_all_probable_solutions(R)')][0]['R']
        }
    else: return {"error": "There are no valid sessions"}

def has_session():
    return len([*prolog.query('has_session')]) != 0
