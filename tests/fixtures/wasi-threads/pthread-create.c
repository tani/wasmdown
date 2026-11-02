/* A real wasi-libc pthread guest for the WASI Preview1 threads lane.
 *
 * Built with wasi-sdk's wasm32-wasip1-threads target, which ships a libc
 * compiled with atomics and pthread support. The initial/max shared
 * memory must be large enough for a pthread stack (the SDK default two
 * pages is not), hence the linker flags in the test harness.
 *
 * Expected stdout: "thread:42\n"; exit status 0.
 */
#include <pthread.h>
#include <stdatomic.h>
#include <stdio.h>

static atomic_int flag;

static void *worker(void *arg)
{
    atomic_store(&flag, (int)(long)arg);
    return NULL;
}

int main(void)
{
    pthread_t thread;
    int created = pthread_create(&thread, NULL, worker, (void *)42L);

    if (created != 0) {
        printf("create:%d\n", created);
        return 1;
    }
    pthread_join(thread, NULL);
    printf("thread:%d\n", atomic_load(&flag));
    return 0;
}
