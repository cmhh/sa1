FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive
ENV SHELL=/bin/bash

RUN apt-get update && \
  apt-get -y dist-upgrade && \
  apt-get --no-install-recommends -y install wget lsb-release software-properties-common locales && \ 
  wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc && \
  add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" && \
  apt-get update && \
  apt-get --no-install-recommends -y install r-base r-base-core r-base-dev r-recommended && \
  echo "MAKE=make -j20" >> /etc/R/Makeconf && \
  R -e "install.packages(c('data.table', 'dplyr', 'stringr', 'RSQLite', 'DBI'))" && \
  apt-get -y remove lsb-release software-properties-common && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/* && \
  sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
  dpkg-reconfigure --frontend=noninteractive locales && \
  update-locale LANG=en_US.UTF-8

COPY R/get_and_tidy_sa1_data.R /
COPY data /data

ENTRYPOINT ["Rscript", "get_and_tidy_sa1_data.R"]