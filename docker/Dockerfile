FROM jfloff/alpine-python:3.6

COPY /deps/requirements.txt /annotald/
RUN pip install -r /annotald/requirements.txt

COPY /deps/lovett-requirements.txt /annotald/lovett-requirements.txt
RUN pip install -r /annotald/lovett-requirements.txt

COPY /deps/lovett-*.tar.gz /annotald/
RUN pip install /annotald/lovett-*.tar.gz

COPY /deps/main.py /annotald/
COPY /deps/static /annotald/static

WORKDIR /data
