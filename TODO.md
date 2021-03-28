# TODO

## Permitted filename chars

On both Windows and unix some characters are disallowed in filenames (and as such in filepaths),
most notably `NUL`. Construction functions could check for these invariants?

* https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file#naming-conventions
* https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_170
