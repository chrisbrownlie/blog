FROM analythium/r2u-quarto:20.04 AS builder

COPY . /app
WORKDIR /app
RUN mkdir output
RUN quarto render --output-dir output

FROM ghcr.io/openfaas/of-watchdog:0.9.6 AS watchdog

FROM alpine:latest
RUN mkdir /app
COPY --from=builder /app/output /app
COPY --from=watchdog /fwatchdog .
ENV mode="static"
ENV static_path="/app"
HEALTHCHECK --interval=3s CMD [ -e /tmp/.lock ] || exit 1
CMD ["./fwatchdog"]
