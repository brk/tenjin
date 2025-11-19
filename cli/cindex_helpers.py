from typing import Generator

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


def render_declaration_sans_qualifiers(type_obj, var_name):
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
