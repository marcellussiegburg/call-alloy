FROM centos:7 AS install
RUN yum -y install java-1.7.0-openjdk-devel \
 && yum clean all \
 && mkdir /work \
 && mv $(find / -name "rt.jar" | head -1) /work/rt.jar
WORKDIR /work

FROM scratch AS export
COPY --from=install /work /
