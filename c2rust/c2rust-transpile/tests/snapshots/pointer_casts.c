#include <stdint.h>

int *integer_to_mut_pointer(uintptr_t address) {
    return (int *)address;
}

const int *integer_to_const_pointer(uintptr_t address) {
    return (const int *)address;
}

uintptr_t pointer_to_integer(const int *pointer) {
    return (uintptr_t)pointer;
}
