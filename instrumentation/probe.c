/*
 * RTSIG shim with cookie acknowledgment.
 */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>

#define __sigin (SIGRTMIN)
#define __sigout (SIGRTMIN + 1)

static void nqsb_trap (int sig, siginfo_t *info, void *p) {
  int res = fork ();
  if (res == -1) {
    perror ("** NQSB: failed to fork");
  }
  if (res == 0) {
    if (sigqueue (info->si_pid, __sigout, (union sigval) info->si_int) == -1) {
      perror ("** NQSB: cannot ack request");
      exit (1);
    }
  }
  kill (getpid (), SIGSTOP);
}

__attribute__((constructor))
void nqsb_instrument () {
  struct sigaction sa;
  sigemptyset (&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO | SA_RESTART;
  sa.sa_sigaction = &nqsb_trap;
  if (sigaction (__sigin, &sa, NULL) == -1) {
    perror ("** NQSB: failed to initialize");
    exit (1);
  }
  printf ("** NQSB: instrumentation active.\n");
}
