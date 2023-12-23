FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive
ENV LC_ALL=C.utf8

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    ruby \
    r-base-core \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /home/${USER}/work

# --------------------------------

ARG USER
ARG GROUP

RUN groupadd ${USER} \
  && useradd ${USER} -g ${GROUP} -m

USER ${USER}

WORKDIR /home/${USER}

ENV USER=${USER}

# --------------------------------

RUN cat <<'__EOS' >> "/home/${USER}/.bashrc"

PS1_ORIG="$PS1"

export PS1='  ---------------- \[\e]0;\u@\h: \w\a\]${debian_chroot:+($debian_chroot)}\u@\h:\w\n  \$ '
__EOS

RUN cat <<'__EOS' >> "/home/${USER}/.bash_history"
rake clean
rake clean build
cat test_common/
cat test_common/compile/01.mrcl 
./test.sh j 1
./test.sh l 1
./test.sh p 1
./test.sh c 27
./test.sh a
__EOS

WORKDIR /home/${USER}/work

ENV IN_CONTAINER=1
