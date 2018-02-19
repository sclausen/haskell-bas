FROM        ubuntu:16.04

ENV         TERM xterm-256color
ENV         DEBIAN_FRONTEND noninteractive

RUN         apt-get update && apt-get install -y openssh-server vim htop locales coreutils less libgmp-dev && \
            apt-get clean && \
            rm -rf /var/lib/apt/lists/*

COPY        docker/locale.gen /etc/locale.gen
#RUN         sed -i -e 's/# de_DE.UTF-8 UTF-8/de_DE.UTF-8 UTF-8/' /etc/locale.gen
#RUN         cp /usr/share/i18n/SUPPORTED /etc/locale.gen
RUN         locale-gen
ENV         LANG en_US.UTF-8
ENV         LANGUAGE en_US:en

COPY        docker/ssh-start /usr/local/bin/
COPY        docker/sshd_config /etc/ssh/sshd_config

RUN         groupadd bas
RUN         useradd -m -s /bin/bash foo
RUN         usermod -p '*' foo
RUN         usermod -aG bas foo

COPY        docker/keys /var/run/sshd/keys
RUN         chmod 755 /var/run/sshd/keys; \
            chmod 644 /var/run/sshd/keys/*

RUN         mkdir -p /etc/bas
COPY        bas.db /etc/bas/
COPY        docker/run-bas.sh /etc/bas/
COPY        docker/scripts /root/
RUN         chown -R root:bas /etc/bas
RUN         chmod -R g+rwx /etc/bas
COPY        .stack-work/dist/x86_64-linux/Cabal-2.0.1.0/build/haskell-bas/haskell-bas /usr/local/bin/
RUN         chown root:bas /usr/local/bin/haskell-bas

EXPOSE      22
ENTRYPOINT  ["/usr/local/bin/ssh-start"]
CMD         ["ssh-server"]