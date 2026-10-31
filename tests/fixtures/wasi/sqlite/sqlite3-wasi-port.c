/* Fixture port source for the SQLite shell on WASI (development only).
 *
 * The shell's INTERACTIVE PROMPT asks gethostname() for the machine name.
 * WASI has no hostname query, and the shell already handles a failed query
 * by falling back to "?". This file supplies exactly that failed query and
 * nothing else: no SQL, VFS, locking or CLI logic is touched, and no fake
 * successful hostname is invented.
 */
#include "sqlite3-wasi-port.h"

int wmd_shell_gethostname(char *name, unsigned long len) {
  (void)name;
  (void)len;
  errno = ENOSYS;
  return -1;
}
