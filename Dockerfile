FROM        ubuntu:16.04

ENV         TERM xterm-256color
ENV         DEBIAN_FRONTEND noninteractive

RUN         apt-get update && apt-get install -y openssh-server vim htop locales coreutils less libgmp-dev && \
            apt-get clean && \
            rm -rf /var/lib/apt/lists/*

RUN         groupadd bas
RUN         useradd -m -s /bin/bash foo
RUN         usermod -p '*' foo
RUN         usermod -aG bas foo
RUN         mkdir -p /var/run/sshd/keys; \
            mkdir /root/.ssh && chmod 700 /root/.ssh; \
            touch /root/.ssh/authorized_keys; \ 
            chmod 600 /root/.ssh/authorized_keys

RUN         sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && locale-gen
ENV         LANG en_US.UTF-8
ENV         LANGUAGE en_US:en

COPY        docker/ssh-start /usr/local/bin/
COPY        docker/sshd_config /etc/ssh/sshd_config
COPY        docker/foo.pub /home/foo/.ssh/authorized_keys

RUN         chown -R foo:foo /home/foo/.ssh
RUN         chmod 700 /home/foo/.ssh
RUN         chmod 600 /home/foo/.ssh/authorized_keys

COPY        bas.db /tmp/bas/bas.db
COPY        docker/run-bas.sh /tmp/bas/
RUN         chown -R root:bas /tmp/bas
RUN         chmod -R g+rwx /tmp/bas
COPY        .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/haskell-bas/haskell-bas /usr/local/bin/

EXPOSE      22
ENTRYPOINT  ["/usr/local/bin/ssh-start"]
CMD         ["ssh-server"]