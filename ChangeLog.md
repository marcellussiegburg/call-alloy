# Changelog for call-alloy

## Unreleased changes

- deprecate 'relToMap'
- provide functions for returning raw output of instances

## Released changes

### 0.2.1.1

- fix errors due to long Alloy code by starting timeout after transferring code

### 0.2.1.0

- enable to abort instance generation early by using timeout

### 0.2.0.6

- allow parsing `'` as part of words.
  (Especially `skolem` may return them if variable names in predicates to check
  are not unique.)
- add version constraint for `Win32`
- allow later versions of `bytestring`
