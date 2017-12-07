import glob
import importlib.util
import json
import os
import uuid

# import click
import hug
import marshmallow.fields as fields
import pygit2

import lovett.corpus
from lovett.format import Json, Deep, _Object   # TODO: don't use _Object


# TODO: make these paths more modular
GIT_PATH = "/home/aecay/projects/chlg/.git"
CORPUS_PATH = "/home/aecay/projects/chlg/parsing"
DICT_FILE = "/home/aecay/projects/chlg/dict.json"
CONFIG_FILE = "/home/aecay/projects/chlg/config.json"
VALIDATORS_PATH = "/home/aecay/projects/chlg/doc/validate.py"

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

def import_validate():
    if not os.path.exists(VALIDATORS_PATH):
        raise Exception("Could not find validator library")

    # Import the validation library each time.  This is to make iterative
    # development simpler
    spec = importlib.util.spec_from_file_location("validate", VALIDATORS_PATH)
    validate = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(validate)
    return validate

@hug.post("/validate")
def do_validate(trees: hug.types.json):
    validate = import_validate()
    c = lovett.corpus.from_objects(trees)
    validate.validate(c)
    return _Object.corpus(c)

def get_id(corpus, tree_id):
    tree = filter(lambda x: x.metadata.id == tree_id, corpus).next()
    return tree

def get_path(tree, path):
    for component in path:
        tree = tree[component]
    return tree

@hug.post("/fix_validator")
def git_info(filename: hug.types.string, trees: hug.types.json,
             validator_name: hug.types.string
             tree_id: hug.types.string, path: fields.List(fields.Int())):
    repo = pygit2.Repository(GIT_PATH)
    # status = repo.status()
    filepath = os.relpath(os.path.join(CORPUS_PATH, filename),
                          repo.workdir)
    # try:
    #     if status[filepath] & (pygit2.GIT_STATUS_WT_NEW |
    #                                pygit2.GIT_STATUS_WT_MODIFIED):
    #         raise Exception("File is dirty")
    # except KeyError:
    #     pass
    validate = import_validate()
    new_corpus = lovett.corpus.from_objects(trees)
    head = repo.lookup_reference("HEAD").get_object()
    trees = head.tree
    dirname = os.path.dirname(filepath)
    if dirname != "":
        for component in dirname.split("/"):
            trees.insert(0, repo[trees[0][component].oid])
    assert len(filepath.split("/")) == len(trees)

    old_text = trees[0][os.path.basename(filepath)].data.decode("utf-8")
    old_corpus = lovett.corpus.from_file(io.StringIO(old_text), Deep)
    old_tree = get_id(old_corpus, tree_id)
    old_target = get_path(old_tree, path)
    new_tree = get_id(new_corpus, tree_id)
    new_target = get_path(new_tree, path)

    validator = validate[validator_name]

    raised = False
    try:
        validator(old_target)
    except AssertionError:
        raised = True

    if not raised:
        raise Exception("Old tree doesn't fail")

    try:
        validator(new_target)
    except AssertionError:
        raise Exception("New tree doesn't pass")

    replacement_corpus = ListCorpus([tree if tree.id != tree_id else new_tree for tree in old_corpus])
    new_blob = repo.create_blob(replacement_corpus.format(Deep).encode("utf-8"))
    for component in filepath.split("/").reverse():
        tb = repo.TreeBuilder(trees.pop(0))
        assert isinstance(to_insert, pygit2.Blob) or isinstance(to_insert, pygit2.Tree)
        tb.insert(component, to_insert,
                  pygit2.GIT_FILEMODE_BLOB if isinstance(to_insert, pygit2.Blob) else pygit2.GIT_FILEMODE_TREE)
        to_insert = tb.write()

    annotald = pygit2.Signature("Annotald", "aaronecay+annotald@gmail.com")
    global_config = pygit2.Config.get_global_config()
    user = pygit2.Signature(global_config["user.name"], global_config["user.email"])


    return {'revision': repo.lookup_reference("HEAD").resolve().target.hex}


# @click.command()
# @click.option("--path", type=click.Path())
# def main(path):
#     global CORPUS_PATH
#     CORPUS_PATH = path


# if __name__ == "__main__":
#     main()
