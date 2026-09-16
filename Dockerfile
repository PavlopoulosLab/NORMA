# NORMA 3.0 — public server image (standard library Python only)
FROM python:3.12-slim
WORKDIR /srv/norma
COPY . /srv/norma
RUN useradd --system --home /srv/norma norma && chown -R norma /srv/norma
USER norma
ENV NORMA_MODE=hosted \
    NORMA_HOST=0.0.0.0 \
    NORMA_PORT=8000
EXPOSE 8000
HEALTHCHECK --interval=60s --timeout=5s CMD python3 -c "import urllib.request,sys; sys.exit(0 if urllib.request.urlopen('http://127.0.0.1:8000/api/health',timeout=4).status==200 else 1)"
CMD ["python3", "backend/server.py", "--no-open"]
