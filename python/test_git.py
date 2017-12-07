import os

import pytest

import pygit2

import git

@pytest.fixture
def repo():
    path = os.path.dirname(__file__)
    path = os.path.join(path, os.pardir)
    path = os.path.join(path, ".git")
    path = os.path.abspath(path)
    return pygit2.Repository(path)

def test_file_at_revision(repo):
    assert git.file_at_revision(repo, "README.md", "3059506") == "TODO\n"
    assert git.file_at_revision(repo, "README.md", "57caae0") == "A rewrite of Annotald\n"

    # With directories in the path
    assert git.file_at_revision(repo, "python/requirements.txt", "57caae0") == \
      "click\nhug\ndecorator\n"

    assert git.file_at_revision(repo, "python/requirements.txt", "5b33f76") == \
      "click\nhug\ndecorator\npygit2\nmarshmallow\npytest\n"
