#!/usr/bin/env zsh
JARS=commons-cli/commons-cli-1.5.0.jar:org.alloytools.alloy.dist.jar

docker buildx build --output . .
javac -target 7 -source 7 -bootclasspath rt.jar -cp ${JARS} \
  alloy/RunAlloy.java -d .
#printf '\x33' | dd of=alloy/RunAlloy.class bs=1 seek=7 count=3 conv=notrunc
