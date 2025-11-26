from typing import Generator
from pycparser import c_lexer  # type: ignore

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


def render_normalized_declaration(decl_sans_semicolon: str) -> tuple[str, set[str]]:
    """Render a normalized declaration string for comparison purposes."""
    tu = TranslationUnit.from_source(
        "example.c",
        args=None,
        unsaved_files=[("example.c", decl_sans_semicolon + ";")],
    )
    prefix_lines = []
    typedef_names = set()
    for diag in tu.diagnostics:
        if diag.severity >= diag.Error:
            sp = diag.spelling
            if sp.startswith("unknown type name"):
                type_name = sp.split("'")[1]
                typedef_names.add(type_name)
                prefix_lines.append(f"typedef int {type_name};")
    if prefix_lines:
        tu = TranslationUnit.from_source(
            "example.c",
            args=None,
            unsaved_files=[
                ("example.c", "\n".join(prefix_lines) + "\n" + decl_sans_semicolon + ";")
            ],
        )

    def annotated(s: str) -> tuple[str, set[str]]:
        return s, typedef_names

    # Get the last declaration in the TU.
    cursor = list(tu.cursor.get_children())[-1]

    # print(cursor.kind, cursor.spelling)
    if cursor.kind == CursorKind.FUNCTION_DECL:
        return annotated(render_declaration_sans_qualifiers(cursor.type, cursor.spelling))

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
                return annotated(render_declaration_sans_qualifiers(cursor.type, cursor.spelling))

    else:
        decl_str = render_declaration_sans_qualifiers(
            cursor.underlying_typedef_type, cursor.spelling
        )
        return annotated("typedef " + decl_str)
    raise ValueError(f"Unexpected cursor kind: {cursor.kind}")


def lex_preprocessed_c(sourcetext: str, typedef_names: set[str]) -> list[str]:
    """Lex preprocessed C source text into tokens using pycparser."""
    lexer = c_lexer.CLexer(
        error_func=lambda msg, line, column: print(f"Lexer error at {line}:{column}: {msg}"),
        on_lbrace_func=lambda: None,
        on_rbrace_func=lambda: None,
        type_lookup_func=lambda name: name in typedef_names,
    )
    lexer.build()
    lexer.input(sourcetext)

    tokens = []
    while True:
        tok = lexer.token()
        if not tok:
            break
        tokens.append(tok.value)
    return tokens


if __name__ == "__main__":
    from clang.cindex import TranslationUnit

    sourcetexts = [
        (
            "int mtimesort(struct _info **a, struct _info **b)",
            "int mtimesort ( struct _info * * , struct _info * * )",
        ),
        (
            "extern int mtimesort(struct _info **a, struct _info **b)",
            "int mtimesort ( struct _info * * , struct _info * * )",
        ),
        (
            "extern int mtimesort(struct _info **a,   struct   _info * * b)",
            "int mtimesort ( struct _info * * , struct _info * * )",
        ),
        ("char const *prot(mode_t m)", "const char * prot ( mode_t )"),
        (
            "extern int (*topsort)(const struct _info **, const struct _info **)",
            "int ( * topsort ) ( const struct _info * * , const struct _info * * )",
        ),
        ("void *xrealloc (void *ptr, size_t size)", "void * xrealloc ( void * , size_t )"),
        ("void printit(const char *s)", "void printit ( const char * )"),
        (
            "typedef int (*compfunc_t)(const void *, const void *)",
            "typedef int ( * compfunc_t ) ( const void * , const void * )",
        ),
    ]
    for sourcetext in sourcetexts:
        txt, typedef_names = render_normalized_declaration(sourcetext[0])
        result = " ".join(lex_preprocessed_c(txt, typedef_names))
        expected = sourcetext[1]
        status = "✓" if result == expected else "✗"
        print(f"{status} {result}")
        if result != expected:
            print(f"  Expected: {expected}")
