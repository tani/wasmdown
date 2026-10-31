/* See sqlite3-wasi-port.c: the only unsupported host query in the shell. */
#ifndef WMD_SQLITE3_WASI_PORT_H
#define WMD_SQLITE3_WASI_PORT_H

#include <errno.h>

int wmd_shell_gethostname(char *name, unsigned long len);

#endif /* WMD_SQLITE3_WASI_PORT_H */
