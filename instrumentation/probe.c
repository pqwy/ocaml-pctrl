#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <sys/socket.h>
#include "sendmsg.h"

#define signo SIGRTMIN
#define bsize 1024

static int ctrlfd = -1,
           datafd = -1;
static char *msgbuf;

static int _err (int res, char *msg) {
  if (res == -1) { perror (msg); exit (1); }
  return res;
}

static int fd_env_lookup (const char *name) {
  char *val = getenv (name);
  int res = -1;
  if (val == NULL || (res = atoi (val)) == -1) {
    printf ("** NQSB: error looking up fd in env var %s.\n", name);
    exit (1);
  }
  return res;
}

static void nqsb_trap (int sig, siginfo_t *info, void *p) {
  int res = _err (fork (), "** NQSB: failed to fork");
  if (res == 0) {
    size_t n = 0;
    int fds[2] = {0};
    _err (socketpair (PF_UNIX, SOCK_STREAM, 0, fds), "** NQSB: socketpair");
    _err (dup2(fds[0], datafd), "** NQSB: dup");
    n = snprintf (msgbuf, bsize, "%d %d\n", info->si_int, getpid ());
    _err (sendmsg_with_sock (ctrlfd, fds[1], (void **) &msgbuf, &n, 1), "** NQSB: failed to ack");
    _err (close (fds[0]), "** NQSB: closing aux socket");
    _err (close (fds[1]), "** NQSB: closing aux socket");
  }
}

__attribute__((constructor))
void nqsb_instrument () {

  datafd = fd_env_lookup ("NQSB_DATA");
  ctrlfd = fd_env_lookup ("NQSB_CONTROL");

  if ((msgbuf = malloc (bsize)) == NULL) {
    printf ("** NQSB: cannot allocate working buffer.\n");
    exit (1);
  }

  struct sigaction sa;
  sigemptyset (&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO | SA_RESTART;
  sa.sa_sigaction = &nqsb_trap;

  _err (sigaction (signo, &sa, NULL),
    "** NQSB: failed to initialize: installing handler.");

  printf ("** NQSB: instrumentation active.\n");
}

