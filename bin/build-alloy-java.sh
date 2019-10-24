#!/usr/bin/env zsh

javac -cp org.alloytools.alloy.dist.jar RunAlloy.java -d .
printf '\x30' | dd of=alloy/RunAlloy.class bs=1 seek=7 count=3 conv=notrunc
