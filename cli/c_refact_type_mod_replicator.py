"""
Type definitions can end up replicated across translation units
for two main reasons:

    - Header files included in multiple source files.
    - Manually duplicated type definitions.

We should handle both cases; regardless of origin, we must maintain
consistency across all instances of a type definition to avoid
undefined behavior:

+--------------------------------------------------------------------+
|                                                                    |
|  When we modify named type definitions in any translation unit,    |
|  we must replicate those changes across every translation unit.    |
|                                                                    |
+--------------------------------------------------------------------+

At least in theory, two #includes of the same file in different
translation units could result in different expanded text. So at least
in theory, one could have a translation unit A that gets definition D
from header H, and another translation unit B that also includes H but
gets an equivalent-but-not-textually-identical definition D elsewhere.
Even without textual identity, macro-expanded definitions can still be
equivalent, either due to int/signed int, or use of typedefs.

So we have two challenges:

    1. Ensure that changes to type definitions are replicated across all
       translation units.

    2. Avoid having type definition changes interfere with refolding.
       Naively, any change to a type definition from a header file
       would prevent refolding the header contents back to an #include.

For challenge 1, we'll require that all type definitions be textually
equivalent across translation units. To avoid trivial whitespace differences
from making definitions appear to differ, we could either
    - compare token sequences rather than raw text, or
    - run `clang-format` to ensure consistent formatting.
        (Something like `" ".join(defntext.split())` does not handle
        things like spaces before semicolons.)
If we could run clang-format on the .i file that would be convenient
but doing so would interfere with refolding. If all definitions were
byte-for-byte identical we could precisely map sub-edits between them,
but if we only care about replicating entire changed definitions, we
only need to know the spans.

For challenge 2, if we change the definition in the header and restore
the un-modified definition in the .nolines.i file, refolding might be
able to restore the #include. HOWEVER other modifications might prevent
the #include from being restored, in which case we want to keep the
modified definition in the .nolines.i file -- otherwise, the modification
will be lost!

    Note that because #includes can be nested and partially restored
    during refolding, it is not super straightforward to determine
    which includes were restored and which were not.


So, for a given codebase, we want to construct a mapping from
type specifiers (e.g. `struct foo`) to a list of every definition
of that type in every translation unit. If any definitions are not
textually identical, we should report an error and abort.

During editing (by BatchingRewriter) we'll track before/after locations
for insertion points of interest (where the "after" location might be
None if the insertion point fell within a modified/deleted span).

After editing, we'll replace each original type definition with a
modified definition. We'll also leave some information for refolding.

During refolding, we'll:
    - Modify the definition in the header file, if there is one.
    - Restore the original definition in the .nolines.i files.
    - Perform refolding.
    - If the un-modified definition remains in the refolded .c file,
      replace it with the modified definition.
"""
