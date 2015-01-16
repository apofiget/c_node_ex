#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "erl_interface.h"
#include "ei.h"

#define BUFSIZE 1000

static char *fns[] = {
  "foo", "bar", "baz", "quax"
};

typedef struct _thread_data_t {
  int fd;
  int idx;
  char *node;
} thread_data_t;

static int my_listen(int);
static short int get_fn_idx(char **, char *);
static void *message_read_loop(void *);

int main(int argc, char **argv) {
  int port;                                /* Listen port number */
  int fd;                                  /* Socket descriptor */
  int listen;                              /* Listen socket */
  ErlConnect conn;                         /* Connection data */
  char *cookie;                            /* Erlang magic cookie */

  pthread_t *thread;
  pthread_attr_t *attr;
  thread_data_t data_t;
  static int tidx = 0;

  if (argc == 3)
    {
      port = atoi(argv[1]);
      cookie = argv[2];
    } else return -1;

  erl_init(NULL, 0);


  if (erl_connect_init(1, cookie, 0) == -1)
    erl_err_quit("erl_connect_init");

  /* Make a listen socket */
  if ((listen = my_listen(port)) <= 0)
    erl_err_quit("my_listen");

  if (erl_publish(port) == -1)
    erl_err_quit("erl_publish");

  if (pthread_attr_init(attr) != NULL) {
    fprintf(stderr, "error while init pthread attr struct\n\r");
    return -1;
  }

  if ((pthread_attr_setdetachstate(attr, PTHREAD_CREATE_DETACHED)) != NULL) {
    fprintf(stderr, "error while set pthread attributes\n\r");
    return -1;
  }

  for(;;) {
    // Change &conn to data_t.conn
  while((fd = erl_accept(listen, &conn)) == ERL_ERROR)
    fprintf(stderr, "%s Connection error\n\r", argv[0]);

  data_t.fd = fd;
  data_t.idx = tidx;
  data_t.node = conn.nodename;

  fprintf(stderr, "Try fork pthread...\n\r");
  if (pthread_create(&thread, attr, message_read_loop, &data_t)) {
    fprintf(stderr, "error: pthread_create\n\r");
    return EXIT_FAILURE;
  }
  tidx++;
 }
  return 0;
}

int my_listen(int port) {
  int listen_fd;
  struct sockaddr_in addr;
  int on = 1;

  if ((listen_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    return (-1);

  setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

  memset((void*) &addr, 0, (size_t) sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  addr.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(listen_fd, (struct sockaddr*) &addr, sizeof(addr)) < 0)
    return (-1);

  listen(listen_fd, 5);
  return listen_fd;
}

short int get_fn_idx(char **funs, char *pattern) {
  int i = 0;

  while(i < sizeof(funs)) {
    if (strcmp(*funs, pattern) == 0)
      return i;
    i++;
    funs++;
  }
  return -1;
}

void *message_read_loop(void *arg) {

  thread_data_t *data_t = (thread_data_t *)arg;

  ErlMessage emsg;                           /* Incoming message */
  unsigned char buf[BUFSIZE];                /* Buffer for incoming message */
  int got;                                   /* Result of receive */
  ETERM *fromp, *tuplep, *fnp, *argp, *resp; /* Erlang terms*/
  int loop = 1;                              /* Loop flag*/
  const char *call;
  short int idx;
  char *atom;

  fprintf(stderr, "[%d] Connection] with node: %s\n\r",data_t->idx, data_t->node);

   while (loop) {

    got = erl_receive_msg(data_t->fd, buf, BUFSIZE, &emsg);

    switch (got) {
    case ERL_TICK:
      fprintf(stderr, "[%d] %s tick\n\r",data_t->idx, data_t->node);
      break;
    case ERL_ERROR:
      fprintf(stderr, "[%d] %s erl_receive_msg error\n\r",data_t->idx, data_t->node);
      loop = 0;
      break;
    case ERL_MSG:
      /*
        Exepected term: {from_pid(), {fun_name_atom, argument}}}
      */
      if (emsg.type == ERL_REG_SEND || emsg.type ==  ERL_SEND) {

        fromp = erl_element(1, emsg.msg);
        tuplep = erl_element(2, emsg.msg);
        fnp = erl_element(1, tuplep);
        argp = erl_element(2, tuplep);

        atom = ERL_ATOM_PTR(fnp);

        if((idx = get_fn_idx(fns, atom)) >= 0)
          call = fns[idx];
        else
          call = "uknown_function";

        fprintf(stderr, "[%d] %s call %s()\n\r",data_t->idx, data_t->node, call);

        if ((resp = erl_format("{cnode, {reply, ~w}}}", erl_mk_atom(call))) != NULL) {
          if(!erl_send(data_t->fd, fromp, resp))
            fprintf(stderr, "[%d] %s send reply error\n\r",data_t->idx, data_t->node);
        } else
          fprintf(stderr, "term format error \n\r");

        erl_free_term(emsg.from);
        erl_free_term(emsg.msg);
        erl_free_term(fromp);
        erl_free_term(tuplep);
        erl_free_term(fnp);
        erl_free_term(argp);
        erl_free_term(resp);
      }
      break;
    default:
      fprintf(stderr, "[%d] %s something wrong! :(\n\r",data_t->idx, data_t->node);
      loop = 0;
      break;
    }
  } /* while */
   fprintf(stderr, "[%d] %s pthread stop\n\r",data_t->idx, data_t->node);
   pthread_exit(NULL);
}
