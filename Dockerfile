###############
### STAGE 0 ###
###############

FROM mcreations/sbcl:1.4.1-ql-2017-12-27-mc-2018-01-17

RUN sbcl --eval "\
  (progn                           \
    (ql:quickload \"deploy\"))     \
  "

COPY . /opt/quicklisp/local-projects/mulkcms

RUN sbcl --eval "\
  (progn                           \
    (ql:quickload \"mulkcms-ht\")) \
  "

USER root
RUN mkdir -p /opt/quicklisp/local-projects/mulkcms/bin
RUN chown lisp /opt/quicklisp/local-projects/mulkcms/bin

RUN sbcl --eval "\
  (progn                           \
    (asdf:make \"mulkcms-ht\"))    \
  "


###############
### STAGE 1 ###
###############

FROM debian:9-slim

RUN apt-get update && apt-get install -y libssl1.0 && rm -rf /var/lib/apt/lists/* && apt-get clean


COPY --from=0 /opt/quicklisp/local-projects/mulkcms/bin /mulkcms/bin
COPY static-files                                       /mulkcms/static-files
COPY templates                                          /mulkcms/templates

WORKDIR /mulkcms
ENTRYPOINT ["/mulkcms/bin/mulkcms-ht"]
