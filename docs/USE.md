# Tenjin User Documentation

N.B. Tenjin is still extremely early stage; these are mostly a placeholder.

## Translation

```
$ 10j translate \
    --codebase <PATH_TO_PROJECT_DIRECTORY> \
    --resultsdir <WHATEVER>
```

The `resultsdir` will contain a series of codebase copies as it undergoes
various transformations during translation. The final version of the code
will be contained in a subdirectory called `final/`.

Currently, Tenjin requires that the codebase being translated live in a
Git or jj repository, with a remote named `origin`. Information about the
state of the translated repository is embedded into generated JSON metadata
files in the `resultsdir`. This metadata is intended to allow Tenjin's
developers to easily replicate the results of translation. Eventually this
metadata will be made optional/opt-out.