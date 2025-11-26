from typing import Generator
from dataclasses import dataclass

from clang.cindex import (  # type: ignore
    TypeKind,
    Cursor,
    CursorKind,
)

type AncestorChain = tuple[Cursor, AncestorChain | None]


def yield_matching_cursors(
    root_cursor: Cursor, cursor_kinds_of_interest: list[CursorKind]
) -> Generator[tuple[Cursor, AncestorChain], None, None]:
    """Yield all call expression cursors in the translation unit."""

    worklist: list[AncestorChain] = [(root_cursor, None)]
    while worklist:
        current, ancestors = worklist.pop()
        if current.kind in cursor_kinds_of_interest:
            assert ancestors is not None
            yield (current, ancestors)

        for child in current.get_children():
            worklist.append((child, (current, ancestors)))  # type: ignore


def render_declaration_sans_qualifiers(type_obj, var_name) -> str:
    """Render a variable declaration for the given type and name."""

    def render_inner(ty, inner_text):
        """Recursively build the declaration string."""
        kind = ty.kind

        if kind == TypeKind.POINTER:
            pointee = ty.get_pointee()
            # Need parentheses if inner_text contains array or function syntax
            if pointee.kind in (
                TypeKind.CONSTANTARRAY,
                TypeKind.FUNCTIONPROTO,
                TypeKind.FUNCTIONNOPROTO,
            ):
                return render_inner(pointee, f"(*{inner_text})")
            else:
                return render_inner(pointee, f"*{inner_text}")

        elif kind == TypeKind.CONSTANTARRAY:
            size = ty.get_array_size()
            element_type = ty.get_array_element_type()
            return render_inner(element_type, f"{inner_text}[{size}]")

        elif kind == TypeKind.INCOMPLETEARRAY:
            element_type = ty.get_array_element_type()
            return render_inner(element_type, f"{inner_text}[]")

        elif kind == TypeKind.FUNCTIONPROTO or kind == TypeKind.FUNCTIONNOPROTO:
            result_type = ty.get_result()
            # Get parameter types
            args_str = ", ".join(arg.spelling for arg in ty.argument_types())
            if not args_str and kind == TypeKind.FUNCTIONNOPROTO:
                args_str = "void"
            return render_inner(result_type, f"{inner_text}({args_str})")

        elif kind == TypeKind.ELABORATED:
            # Unwrap elaborated types
            named_type = ty.get_named_type()
            return render_inner(named_type, inner_text)

        else:
            # Base case: simple type
            return f"{ty.spelling} {inner_text}"

    return render_inner(type_obj, var_name).strip()


def render_normalized_declaration(decl_sans_semicolon: str) -> str:
    """Render a normalized declaration string for comparison purposes."""
    tu = TranslationUnit.from_source(
        "example.c",
        args=None,
        unsaved_files=[("example.c", sourcetext)],
    )
    prefix_lines = []
    for diag in tu.diagnostics:
        if diag.severity >= diag.Error:
            sp = diag.spelling
            if sp.startswith("unknown type name"):
                type_name = sp.split("'")[1]
                prefix_lines.append(f"typedef int {type_name};")
    if prefix_lines:
        tu = TranslationUnit.from_source(
            "example.c",
            args=None,
            unsaved_files=[("example.c", "\n".join(prefix_lines) + "\n" + sourcetext)],
        )

    # Get the last declaration in the TU.
    cursor = list(tu.cursor.get_children())[-1]

    # print(cursor.kind, cursor.spelling)
    if cursor.kind == CursorKind.FUNCTION_DECL:
        return render_declaration_sans_qualifiers(cursor.type, cursor.spelling)

    elif cursor.kind == CursorKind.VAR_DECL:
        ty = cursor.type
        if ty.kind == TypeKind.POINTER:
            pointee = ty.get_pointee()
            # Need parentheses if inner_text contains array or function syntax
            if pointee.kind in (
                TypeKind.CONSTANTARRAY,
                TypeKind.FUNCTIONPROTO,
                TypeKind.FUNCTIONNOPROTO,
            ):
                return render_declaration_sans_qualifiers(cursor.type, cursor.spelling)

    else:
        decl_str = render_declaration_sans_qualifiers(
            cursor.underlying_typedef_type, cursor.spelling
        )
        return "typedef " + decl_str
    raise ValueError(f"Unexpected cursor kind: {cursor.kind}")


if __name__ == "__main__":
    from clang.cindex import TranslationUnit

    sourcetexts = [
        "int mtimesort(struct _info **a, struct _info **b);",
        "extern int mtimesort(struct _info **a, struct _info **b);",
        "extern int mtimesort(struct _info **a,   struct   _info * * b);",
        "char const *prot(mode_t m);",
        "extern int (*topsort)(const struct _info **, const struct _info **);",
        "void *xrealloc (void *ptr, size_t size);",
        "void printit(const char *s);",
        "typedef int (*compfunc_t)(const void *, const void *);",
    ]
    for sourcetext in sourcetexts:
        print(render_normalized_declaration(sourcetext))
