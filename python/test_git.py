import os

import pytest

import pygit2

import git_fns


@pytest.fixture
def repo():
    path = os.path.dirname(__file__)
    path = os.path.join(path, os.pardir)
    path = os.path.join(path, ".git")
    path = os.path.abspath(path)
    return pygit2.Repository(path)


def test_file_at_revision(repo):
    assert git_fns.file_at_revision(repo, "README.md", "3059506") == "TODO\n"
    assert git_fns.file_at_revision(repo, "README.md", "57caae0") == "A rewrite of Annotald\n"

    # With directories in the path
    assert git_fns.file_at_revision(repo, "python/requirements.txt", "57caae0") == \
        "click\nhug\ndecorator\n"

    assert git_fns.file_at_revision(repo, "python/requirements.txt", "5b33f76") == \
        "click\nhug\ndecorator\npygit2\nmarshmallow\npytest\n"


def test_write_file(git_repo):
    path = git_repo.workspace

    file_name = path / "foo.txt"
    file_name.write_text("foobar")
    git_repo.api.index.add([file_name])
    git_repo.api.index.commit("first commit")

    repo = pygit2.Repository(path)
    head0 = repo.revparse_single("HEAD")
    new_tree = git_fns.write_file(repo, head0.tree, "foo/README", "test 1")
    test_sig = pygit2.Signature("test", "test@test.com")
    repo.create_commit("refs/heads/master",
                       test_sig, test_sig,
                       "Test commit 1",
                       new_tree, [head0.oid])

    head1 = repo.revparse_single("HEAD")
    head_tree = head1.tree
    new_tree = git_fns.write_file(repo, head_tree, "foo/README", "test 2")
    repo.create_commit("refs/heads/master",
                       test_sig, test_sig,
                       "Test commit 2",
                       new_tree, [head1.oid])
    head2 = repo.revparse_single("HEAD")

    assert git_fns.file_at_revision(repo, "foo/README", head1.oid.hex) == "test 1"
    assert git_fns.file_at_revision(repo, "foo/README", head2.oid.hex) == "test 2"

    tree = head2.tree
    assert set((entry.name for entry in tree)) == set(("foo", "foo.txt"))
