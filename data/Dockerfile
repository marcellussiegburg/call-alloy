# checkov:skip=CKV_DOCKER_2 This Dockerfile is not intended to be run as a container
# checkov:skip=CKV_DOCKER_3 This Dockerfile is not intended to be run as a container
FROM centos:7 AS install
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN yum -y install java-1.7.0-openjdk-devel \
 && yum clean all \
 && mkdir /work \
 && mv "$(find / -name 'rt.jar' | head -1)" /work/rt.jar
WORKDIR /work

FROM scratch AS export
COPY --from=install /work /
