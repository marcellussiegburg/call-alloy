# Changelog for call-alloy

## Unreleased changes

## Released changes

### 0.3.0.1

- support "State0" line in retrieved instances (introduced by Alloy 6)
- support CRLF at line end

### 0.3

- upgrade to Alloy 6.0.0
- allow slashes in object names
- allow identity relations
- improve feedback on misspelled/missing relation and signature names
- remove 'relToMap'

### 0.2.2.0

- deprecate 'relToMap'
- provide functions for returning raw output of instances
- provide functions for typed retrieval
- deprecate 'getSingle', 'getDouble', 'getTriple', 'objectName'

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
