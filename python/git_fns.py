import os

import pygit2
from typeguard import typechecked


@typechecked
def file_at_revision(repo: pygit2.Repository,
                     filepath: str,
                     revision: str):
    tree = repo.revparse_single(revision).tree
    dirname = os.path.dirname(filepath)
    if dirname != "":
        for component in dirname.split("/"):
            tree = repo[tree[component].oid]

    return repo[tree[os.path.basename(filepath)].id].data.decode("utf-8")


@typechecked
def write_file(repo: pygit2.Repository,
               tree: pygit2.Tree,
               filepath: str,
               contents: str) -> pygit2.Oid:
    blob = repo.create_blob(contents.encode("utf-8"))
    paths = filepath.split("/")
    trees = [tree]
    for path in paths[:-1]:
        try:
            to_insert = repo[trees[0][path].oid]
        except KeyError:
            to_insert = None
        trees.insert(0, to_insert)

    to_insert = blob
    for path in reversed(paths):
        tree = trees.pop(0)
        assert isinstance(to_insert, pygit2.Oid)
        assert isinstance(repo[to_insert], pygit2.Blob) or isinstance(repo[to_insert], pygit2.Tree)
        if tree is None:
            tb = repo.TreeBuilder()
        else:
            tb = repo.TreeBuilder(tree)
        tb.insert(path, to_insert,
                  pygit2.GIT_FILEMODE_BLOB if isinstance(repo[to_insert], pygit2.Blob) else pygit2.GIT_FILEMODE_TREE)
        to_insert = tb.write()

    assert len(trees) == 0

    return to_insert
