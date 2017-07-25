import glob
import json
import os

import click
import hug

import lovett.corpus


CORPUS_PATH = "/home/aecay/projects/chlg/parsing"
DICT_FILE = "/home/aecay/projects/chlg/dict.json"

with open(DICT_FILE, "r") as fin:
    DICT = json.loads(fin.read())


# html = hug.get(output=hug.output_format.html)
# @hug.get('/')
# def index():
#     with open('static/index.html') as fin:
#         return fin.read()


@hug.static("/static")
def static():
    return (os.path.join(os.path.dirname(__file__), "static"),)


@hug.get('/files')
def files():
    return list(map(lambda x: os.path.relpath(x, CORPUS_PATH),
                    glob.glob(os.path.join(CORPUS_PATH, "*.psd"))))


@hug.get("/file")
def get_file(name: hug.types.text):
    path = os.path.join(CORPUS_PATH, name)
    with open(path) as fin:
        corpus = lovett.corpus.from_file(fin)
    return json.loads(corpus.to_json())


@hug.post("/save")
def save(filename: hug.types.text, trees: hug.types.json):
    c = lovett.corpus.from_objects(trees)
    for t in c:
        t.metadata.id = "foo"
    with open(os.path.join(CORPUS_PATH, filename), "w") as fout:
        c.write_penn_treebank(fout)


@hug.get("/dictentry")
def get_dict_entry(lemma: hug.types.text):
    return DICT.get(lemma, "")


@hug.post("/dictentry")
def set_dict_entry(lemma: hug.types.text, definition: hug.types.text):
    DICT[lemma] = definition
    with open(DICT_FILE, "w") as fout:
        fout.write(json.dumps(DICT, sort_keys=True, indent=4))


# @click.command()
# @click.option("--path", type=click.Path())
# def main(path):
#     global CORPUS_PATH
#     CORPUS_PATH = path


# if __name__ == "__main__":
#     main()
