# check-spelling/check-spelling configuration

| File                    | Purpose                                                           | Format                                | Info            |
| ----------------------- | ----------------------------------------------------------------- | ------------------------------------- | --------------- |
| dictionary.txt          | Replacement dictionary (creating this file will override default) | one word per line                     | [dictionary][1] |
| allow.txt               | Add words to the dictionary                                       | one word per line                     | [allow][2]      |
| reject.txt              | Remove words from the dictionary (after allow)                    | grep pattern matching                 | [reject][3]     |
| excludes.txt            | Files to ignore entirely                                          | perl regular expression               | [excludes][4]   |
| only.txt                | Only check matching files (applied after excludes)                | perl regular expression               | [only][5]       |
| patterns.txt            | Patterns to ignore from checked lines                             | perl regular expression               | [patterns][6]   |
| candidate.patterns      | Patterns worth adding to patterns.txt                             | perl regular expression with comments | [candidates][7] |
| line_forbidden.patterns | Patterns to flag in checked lines                                 | perl regular expression               | [patterns][6]   |
| expect.txt              | Expected words not in dictionary                                  | one word per line                     | [expect][8]     |
| advice.md               | Supplement for GitHub comment when unrecognized words found       | GitHub Markdown                       | [advice][9]     |

Note: you can replace any of these files
with a directory by the same name (minus the suffix)
and then include multiple files inside that directory (with that suffix)
to merge multiple files together.

[1]: https://github.com/check-spelling/check-spelling/wiki/Configuration#dictionary
[2]: https://github.com/check-spelling/check-spelling/wiki/Configuration#allow
[3]: https://github.com/check-spelling/check-spelling/wiki/Configuration-Examples%3A-reject
[4]: https://github.com/check-spelling/check-spelling/wiki/Configuration-Examples%3A-excludes
[5]: https://github.com/check-spelling/check-spelling/wiki/Configuration-Examples%3A-only
[6]: https://github.com/check-spelling/check-spelling/wiki/Configuration-Examples%3A-patterns
[7]: https://github.com/check-spelling/check-spelling/wiki/Feature:-Suggest-patterns
[8]: https://github.com/check-spelling/check-spelling/wiki/Configuration#expect
[9]: https://github.com/check-spelling/check-spelling/wiki/Configuration-Examples%3A-advice
