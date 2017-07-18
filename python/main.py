# filename: main.py
import hug
import click
import lovett.corpus
import json

import glob
import os


CORPUS_PATH = "/home/aecay/projects/chlg/parsing"


html = hug.get(output=hug.output_format.html)


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


@click.command()
@click.option("--path", type=click.Path())
def main(path):
    global CORPUS_PATH
    CORPUS_PATH = path


if __name__ == "__main__":
    main()
