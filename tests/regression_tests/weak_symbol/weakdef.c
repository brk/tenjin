/* A weak default implementation, overridden by the strong definition of the
   same name in strongdef.c. */
int __attribute__((weak)) impl(void) { return 1; }

/* A weak definition that nothing overrides, so it must survive. */
int __attribute__((weak)) fallback(void) { return 7; }

/* Weak on both sides: the linker keeps whichever object it sees first, which
   is this one, since the link line lists weakdef.o before strongdef.o. */
int __attribute__((weak)) pick(void) { return 100; }

int dispatch(void) { return impl() + fallback() + pick(); }
