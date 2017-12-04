import glob
import importlib.util
import json
import os
import uuid

# import click
import hug

import lovett.corpus
from lovett.format import Json, Deep, _Object   # TODO: don't use _Object


CORPUS_PATH = "/home/aecay/projects/chlg/parsing"
DICT_FILE = "/home/aecay/projects/chlg/dict.json"
CONFIG_FILE = "/home/aecay/projects/chlg/config.json"
VALIDATORS_PATH = "/home/aecay/projects/chlg/doc/validate.py"

validate = None

if os.path.exists(VALIDATORS_PATH):
    spec = importlib.util.spec_from_file_location("validate", VALIDATORS_PATH)
    validate = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(validate)

with open(DICT_FILE, "r") as fin:
    DICT = json.load(fin)


@hug.static("/static")
def static():
    return (os.path.join(os.path.dirname(__file__), "static"),)


@hug.get('/files')
def files():
    return list(map(lambda x: os.path.relpath(x, CORPUS_PATH),
                    glob.glob(os.path.join(CORPUS_PATH, "*.psd"))))


@hug.get("/config")
def config():
    with open(CONFIG_FILE) as fin:
        return json.load(fin)


@hug.get("/file")
def get_file(name: hug.types.text):
    path = os.path.join(CORPUS_PATH, name)
    with open(path) as fin:
        corpus = lovett.corpus.from_file(fin, Deep)
    return _Object.corpus(corpus)


@hug.post("/save")
def save(filename: hug.types.text, trees: hug.types.json):
    c = lovett.corpus.from_objects(trees)
    with open(os.path.join(CORPUS_PATH, filename), "w") as fout:
        fout.write(c.format(lovett.format.Deep))


@hug.get("/dictentry")
def get_dict_entry(lemma: hug.types.text):
    return DICT.get(lemma, "")


@hug.post("/dictentry")
def set_dict_entry(lemma: hug.types.text, definition: hug.types.text):
    DICT[lemma] = definition
    print("Saved definition of '%s' as '%s'" % (lemma, definition))
    with open(DICT_FILE, "w") as fout:
        fout.write(json.dumps(DICT, sort_keys=True, indent=4))

@hug.post("/as_text")
def as_text(tree: hug.types.json):
    t = lovett.tree.from_object(tree)
    return t.format(lovett.format.Penn)

@hug.post("/validate")
def do_validate(trees: hug.types.json):
    c = lovett.corpus.from_objects(trees)
    validate.validate(c)
    return _Object.corpus(c)


# @click.command()
# @click.option("--path", type=click.Path())
# def main(path):
#     global CORPUS_PATH
#     CORPUS_PATH = path


# if __name__ == "__main__":
#     main()
