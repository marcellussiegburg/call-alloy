# Changelog for call-alloy

## Unreleased changes

## Released changes

### 0.2.1.0

- enable to abort instance generation early by using timeout

### 0.2.0.6

- allow parsing `'` as part of words.
  (Especially `skolem` may return them if variable names in predicates to check
  are not unique.)
- add version constraint for `Win32`
- allow later versions of `bytestring`
