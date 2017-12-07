import os

import pygit2

def file_at_revision(repo, filepath, revision):
    tree = repo.revparse_single(revision).tree
    dirname = os.path.dirname(filepath)
    if dirname != "":
        for component in dirname.split("/"):
            tree = repo[tree[component].oid]

    return repo[tree[os.path.basename(filepath)].id].data.decode("utf-8")
