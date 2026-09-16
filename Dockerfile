# NORMA 3 — public server image. Stage 1 builds the page, stage 2 serves it
# with the standard-library Python server (no Python dependencies).
FROM node:22-slim AS build
WORKDIR /build
COPY frontend/package.json frontend/package-lock.json frontend/
RUN cd frontend && npm ci --no-audit --no-fund
COPY frontend frontend
COPY examples examples
RUN cd frontend && npm run build

FROM python:3.12-slim
WORKDIR /srv/norma
COPY backend backend
COPY examples examples
COPY norma.config.json norma.config.hosted.json ./
COPY --from=build /build/frontend/dist frontend/dist
RUN useradd --system --home /srv/norma norma && chown -R norma /srv/norma
USER norma
ENV NORMA_MODE=hosted \
    NORMA_HOST=0.0.0.0 \
    NORMA_PORT=8000
EXPOSE 8000
HEALTHCHECK --interval=60s --timeout=5s CMD python3 -c "import urllib.request,sys; sys.exit(0 if urllib.request.urlopen('http://127.0.0.1:8000/api/health',timeout=4).status==200 else 1)"
CMD ["python3", "backend/server.py", "--no-open"]
