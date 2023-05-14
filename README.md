# Prolog × Wordle

An implementation of the popular word game Wordle with prolog and FastAPI.

## Development
Assuming you have the recommended version of SWI-Prolog and Python...
- Clone this repository
- Create a Python environment
- `pip install -r req.txt`
- `uvicorn main:app --reload`
- You may need to check [this](https://stackoverflow.com/questions/75785959/assertion-failed-when-using-pyswip) if you get Assertion failed because of `pyswip`