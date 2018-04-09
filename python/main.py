import asyncio
from concurrent.futures import ThreadPoolExecutor

import glob
import importlib.util
# import io
import json
import os

# import click
from aiohttp import web
# import marshmallow.fields as fields
# import pygit2
from unidecode import unidecode


import lovett.corpus
from lovett.format import Deep, _Object   # TODO: don't use _Object

# import git_fns

executor = ThreadPoolExecutor(max_workers=10)
loop = asyncio.get_event_loop()


# TODO: make these paths more modular
GIT_PATH = "/home/aecay/projects/chlg/.git"
CORPUS_PATH = "/home/aecay/projects/chlg/parsing"
DICT_FILE = "/home/aecay/projects/chlg/dict.json"
CONFIG_FILE = "/home/aecay/projects/chlg/config.json"
VALIDATORS_PATH = "/home/aecay/projects/chlg/doc/validate.py"
VALIDATOR_DB_PATH = "/home/aecay/projects/chlg/validators.json"

# signature_annotald = pygit2.Signature("Annotald", "aaronecay+annotald@gmail.com")
# global_config = pygit2.Config.get_global_config()
# signature_user = pygit2.Signature(global_config["user.name"],
#                                   global_config["user.email"])

with open(DICT_FILE, "r") as fin:
    DICT = json.load(fin)


routes = web.RouteTableDef()


@routes.get("/")
def root(request):
    return web.HTTPMovedPermanently("/static/index.html")


@routes.get('/files')
def files(request):
    return web.Response(text=json.dumps(list(map(lambda x: os.path.relpath(x, CORPUS_PATH),
                                                 glob.glob(os.path.join(CORPUS_PATH, "*.psd"))))))


@routes.get("/config")
def config(request):
    with open(CONFIG_FILE) as fin:
        return web.Response(text=fin.read())


@routes.get("/lemmata")
def lemmata():
    return web.response(text=json.dumps([{"original": lemma, "normalized": unidecode(lemma)}
                                         for lemma in DICT.keys()]))


@routes.get("/file")
def get_file(request):
    name = request.query["name"]
    path = os.path.join(CORPUS_PATH, name)
    with open(path) as fin:
        corpus = lovett.corpus.from_file(fin, Deep)
    return web.json_response(text=json.dumps(_Object.corpus(corpus)))


@routes.post("/save")
async def save(request):
    data = await request.json()
    filename = data["filename"]
    trees = data["trees"]
    c = lovett.corpus.from_objects(trees)
    for tree in c:
        for node in tree.nodes():
            for key in ("VALIDATION-ERROR", "VALIDATOR-NAME"):
                if key in node.metadata:
                    del node.metadata[key]
        if tree.metadata.id == "MISSING_ID":
            tree.metadata.id = lovett.util.fresh_id()  # TODO: won't be
            # reflected in the Elm version -- reload trees after save?
    with open(os.path.join(CORPUS_PATH, filename), "w") as fout:
        fout.write(c.format(lovett.format.Deep))

    return web.json_response({})


@routes.get("/dictentry")
def get_dict_entry(request):
    return web.json_response(DICT.get(request.query["lemma"], ""))


@routes.post("/dictentry")
async def set_dict_entry(request):
    data = await request.json()
    lemma = data["lemma"]
    definition = data["definition"]
    DICT[lemma] = definition
    print("Saved definition of '%s' as '%s'" % (lemma, definition))
    with open(DICT_FILE, "w") as fout:
        fout.write(json.dumps(DICT, sort_keys=True, indent=4))

    return web.json_response({})


@routes.post("/as_text")
async def as_text(request):
    data = await request.json()
    tree = data["tree"]
    t = lovett.tree.from_object(tree)
    return web.json_response(t.format(lovett.format.Penn))


def import_validate():
    if not os.path.exists(VALIDATORS_PATH):
        raise Exception("Could not find validator library")

    # Import the validation library each time.  This is to make iterative
    # development simpler
    spec = importlib.util.spec_from_file_location("validate", VALIDATORS_PATH)
    validate = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(validate)
    return validate


@routes.post("/validate")
async def do_validate(request):
    data = await request.json()
    trees = data["trees"]
    validate = import_validate()
    c = lovett.corpus.from_objects(trees)
    await loop.run_in_executor(executor, validate.validate, c)
    return web.Response(text=json.dumps(_Object.corpus(c)))


def server(_argv):
    app = web.Application(client_max_size=1024 * 1024 * 1024)
    app.router.add_routes(routes)
    app.router.add_static("/static",
                          os.path.join(os.path.dirname(__file__), "static"),
                          follow_symlinks=True,
                          append_version=True)

    return app


# def get_id(corpus, tree_id):
#     tree = filter(lambda x: x.metadata.id == tree_id, corpus).next()
#     return tree


# def get_path(tree, path):
#     for component in path:
#         tree = tree[component]
#     return tree


# @hug.post("/fix_validator")
# def git_info(filename: hug.types.text, trees: hug.types.json,
#              validator_name: hug.types.text,
#              tree_id: hug.types.text, path: fields.List(fields.Int())):
#     print(filename, validator_name, tree_id, path)
#     return
#     # TODO: verify that the file is git-controlled
#     repo = pygit2.Repository(GIT_PATH)
#     filepath = os.relpath(os.path.join(CORPUS_PATH, filename),
#                           repo.workdir)
#     validate = import_validate()
#     new_corpus = lovett.corpus.from_objects(trees)
#     old_corpus = lovett.corpus.from_file(
#         io.StringIO(git_fns.file_at_revision(repo, filepath, "HEAD")),
#         Deep)
#     old_tree = get_id(old_corpus, tree_id)
#     old_target = get_path(old_tree, path)
#     new_tree = get_id(new_corpus, tree_id)
#     new_target = get_path(new_tree, path)

#     validator = validate[validator_name]

#     raised = False
#     try:
#         validator(old_target)
#     except AssertionError:
#         raised = True

#     if not raised:
#         raise Exception("Old tree doesn't fail")

#     try:
#         validator(new_target)
#     except AssertionError:
#         raise Exception("New tree doesn't pass")

#     replacement_corpus = lovett.corpus.ListCorpus(
#         [tree if tree.id != tree_id else new_tree for tree in old_corpus])

#     head = repo.revparse_single("HEAD")
#     tree = head.tree
#     tree = git_fns.write_file(repo, tree, filepath,
#                               replacement_corpus.format(Deep))

#     commit = repo.create_commit("refs/heads/master",
#                                 signature_annotald, signature_user,
#                                 """Annotald-generated validation sample

# Validator {validator_name} run against tree {tree_id} at {path}""".format(
#                                     validator_name=validator_name,
#                                     tree_id=tree_id,
#                                     path=path),
#                                 tree,
#                                 [head.oid])

#     with open(VALIDATOR_DB_PATH) as fin:
#         db = json.read(fin)
#     db.append({'validator': validator_name,
#                'commit_bad': str(head.oid),
#                'commit_good': str(commit),
#                'tree': tree_id,
#                'path': path})

#     tree = repo[commit]
#     tree = git_fns.write_file(repo, tree, os.relpath(VALIDATOR_DB_PATH,
#                                                      repo.workdir),
#                               json.dumps(db, indent=4))
#     repo.create_commit("refs/heads/master",
#                        signature_annotald, signature_user,
#                        "Update validation database for commit %s" % str(commit)[0:8],
#                        tree, [commit])

# @click.command()
# @click.option("--path", type=click.Path())
# def main(path):
#     global CORPUS_PATH
#     CORPUS_PATH = path


# if __name__ == "__main__":
#     main()
