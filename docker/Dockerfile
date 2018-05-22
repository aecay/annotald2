FROM python:3.6

COPY /requirements.txt /python
RUN pip install -r /python/requirements.txt

COPY /lovett/requirements.txt /python/lovett/requirements.txt
RUN pip install -r /python/lovett/requirements.txt

COPY /lovett /python/lovett
RUN pip install /python/lovett

COPY /main.py /python
COPY /static /python/static

EXPOSE 8000

ENTRYPOINT annotald
