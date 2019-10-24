# `call-alloy` [![Build Status](https://travis-ci.org/marcellussiegburg/call-alloy.svg?branch=master)](https://travis-ci.org/marcellussiegburg/call-alloy)

This is a simple library to call [Alloy](http://alloytools.org) given a specification.
This package includes a simple Java Library to make an API call to the Alloy Library.
Alloy is included (as JAR file) within this library as well.

## Requriements

- Java Runtime Environment:
  There is currently no warning if you have not set up any Java Runtime Environment.
  However, you will get runtime errors if it is not availible when a call to Alloy happens.
  If you want to force a check, perform the test cases.

## Please note

The Java interface to get Alloy instances as well as the Alloy Jar file are backed into this library.

On every call the application checks the [`XdgDirectory`](https://hackage.haskell.org/package/directory/docs/System-Directory.html#t:XdgDirectory) if the libraries exist in a current version.
If not they are placed there together with a version identifier.
