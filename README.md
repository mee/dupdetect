# dupdetect

Detect duplicate directories and files.

When told to only compare file size, is fast (eg. 1.3sec to compare a
100G directory with 13k files and 2k directories).

# usage

    dupdetect: help
      -d DIR  --directory=DIR  top directory
      -v      --verbose        be verbose
      -f      --files          also display duplicate files
      -s      --size-only      compare files by size only
      -h      --help           display help

# todo

## lazy hashing

Automatically compare on size and only compute the hash of a file
when we need to, exploiting laziness.

## fuzzy matching

Detect when directories "mostly" match. For example, one directory has
a few extra files or has the same number of files but the files are
slightly different sizes.

## speed

Hashing is very, very slow.

## output

Make it prettier, configurable, more machine-parseable

# author

Mike Erickson <mike.erickson@gmail.com>
