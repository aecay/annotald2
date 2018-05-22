#!/usr/bin/env python

import asyncio
from concurrent.futures import ThreadPoolExecutor

import glob
import importlib.util
import json
import os

import click
import aiofiles
from aiohttp import web
from unidecode import unidecode

import lovett.corpus
from lovett.format import Deep, _Object   # TODO: don't use _Object

executor = ThreadPoolExecutor(max_workers=10)
loop = asyncio.get_event_loop()

# Helper functions

def import_validate(path):
    if not os.path.exists(path):
        raise Exception("Could not find validator library")

    # Import the validation library each time.  This is to make iterative
    # development simpler
    spec = importlib.util.spec_from_file_location("validate", path)
    validate = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(validate)
    return validate

def long_running(*args):
    return loop.run_in_executor(executor, *args)

def pre_save_handler(corpus):
    for tree in corpus:
        for node in tree.nodes():
            for key in ("VALIDATION-ERROR", "VALIDATOR-NAME"):
                if key in node.metadata:
                    del node.metadata[key]

        if tree.metadata.id == "MISSING_ID":
            # TODO: won't be reflected in the Elm version -- reload trees after save?
            tree.metadata.id = lovett.util.fresh_id()

# Global handlers

def root(request):
    return web.HTTPMovedPermanently("/static/index.html")

async def as_text(request):
    data = await request.json()
    tree = data["tree"]
    t = lovett.tree.from_object(tree)
    # TODO: remove validation errors from result
    return web.json_response({"penn": t.format(lovett.format.Penn),
                              "deep": t.format(lovett.format.Deep),
                              "text": t.urtext})

# Handler object

class Annotald:
    def __init__(self, psd_dir, config_file, dict_file=None, validator_file=None):
        self.psd_dir = psd_dir
        if dict_file is not None:
            with open(dict_file, "r") as fin:
                self.dict = json.load(fin)
        else:
            self.dict = {}

        self.config_file = config_file
        self.validator_file = validator_file

    def files(self, request):
        return web.Response(text=json.dumps(list(map(lambda x: os.path.relpath(x, self.psd_dir),
                                                     glob.glob(os.path.join(self.psd_dir, "*.psd"))))))

    async def config(self, request):
        async with aiofiles.open(self.config_file) as fin:
            t = await fin.read()
            return web.Response(text=t)

    def lemmata(self, request):
        return web.Response(text=json.dumps([{"original": lemma, "normalized": unidecode(lemma)}
                                            for lemma in self.dict.keys()]))

    async def file(self, request):
        name = request.query["name"]
        path = os.path.join(self.psd_dir, name)
        async with aiofiles.open(path) as fin:
            t = await fin.read()
            corpus = lovett.corpus.from_file(t, Deep)
        return web.json_response(text=json.dumps(_Object.corpus(corpus)))


    async def save(self, request):
        data = await request.json()
        filename = data["filename"]
        trees = data["trees"]
        c = lovett.corpus.from_objects(trees)
        await long_running(pre_save_handler, c)
        async with aiofiles.open(os.path.join(self.psd_dir, filename), "w") as fout:
            await fout.write(c.format(lovett.format.Deep))

        return web.json_response({})

    def get_dict_entry(self, request):
        return web.json_response(self.dict.get(request.query["lemma"], ""))

    async def set_dict_entry(self, request):
        data = await request.json()
        lemma = data["lemma"]
        definition = data["definition"]
        self.dict[lemma] = definition
        print("Saved definition of '%s' as '%s'" % (lemma, definition))
        async with aiofiles.open(self.dict_file, "w") as fout:
            await fout.write(json.dumps(self.dict, sort_keys=True, indent=4))

        return web.json_response({})

    async def validate(self, request):
        data = await request.json()
        trees = data["trees"]
        validate = import_validate(self.validator_file)
        c = lovett.corpus.from_objects(trees)
        await long_running(validate.validate, c)
        return web.Response(text=json.dumps(_Object.corpus(c)))

# Entry point

@click.command()
@click.argument("psd_dir",              type=click.Path(file_okay=False, dir_okay=True))
@click.option("--config-file", "-c",    type=click.Path(file_okay=True,  dir_okay=False))
@click.option("--dict-file", "-d",      type=click.Path(file_okay=True,  dir_okay=False), default=None)
@click.option("--validator-file", "-v", type=click.Path(file_okay=True,  dir_okay=False), default=None)
def main(**kwargs):
    annotald = Annotald(**kwargs)
    app = web.Application(client_max_size=1024 * 1024 * 1024)
    app.router.add_routes([web.get("/", root),
                           web.get("/files", annotald.files),
                           web.get("/config", annotald.config),
                           web.get("/lemmata", annotald.lemmata),
                           web.get("/file", annotald.file),
                           web.post("/save", annotald.save),
                           web.get("/dictentry", annotald.get_dict_entry),
                           web.post("/dictentry", annotald.set_dict_entry),
                           web.post("/as_text", as_text),
                           web.post("/validate", annotald.validate)])

    app.router.add_static("/static",
                          os.path.join(os.path.dirname(__file__), "static"),
                          follow_symlinks=True, append_version=True)

    web.run_app(app, host="localhost", port=8000)

if __name__ == "__main__":
    main()
