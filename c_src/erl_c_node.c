#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "erl_interface.h"
#include "ei.h"

#define BUFSIZE 1000

static char *fns[] = {
    "foo", "bar", "baz", "quax"
  };

static int my_listen(int);
static short int get_fn_idx(char **, char *);

int main(int argc, char **argv) {
  int port;                                /* Listen port number */
  int listen;                              /* Listen socket */
  int fd;                                  /* fd to Erlang node */
  ErlConnect conn;                         /* Connection data */

  int loop = 1;                            /* Loop flag */
  int got;                                 /* Result of receive */
  unsigned char buf[BUFSIZE];              /* Buffer for incoming message */
  ErlMessage emsg;                         /* Incoming message */

  ETERM *fromp, *tuplep, *fnp, *argp, *resp;
  const char *res;
  char *atom, *cookie;
  short int idx;

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

  if ((fd = erl_accept(listen, &conn)) == ERL_ERROR)
    erl_err_quit("erl_accept");
  fprintf(stderr, "%s Connected to %s\n\r", argv[0] , conn.nodename);

  while (loop) {

    got = erl_receive_msg(fd, buf, BUFSIZE, &emsg);

    switch (got) {
    case ERL_TICK:
      fprintf(stderr, "%s tick\n\r", argv[0]);
      break;
    case ERL_ERROR:
      fprintf(stderr, "%s erl_receive_msg error\n\r", argv[0]);
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
          res = fns[idx];
        else
          res = "uknown_function";

        fprintf(stderr, "%s call %s()\n\r", argv[0], res);

        if ((resp = erl_format("{cnode, {reply, ~w}}}", erl_mk_atom(res))) != NULL) {
          if(!erl_send(fd, fromp, resp))
            fprintf(stderr, "%s send reply error\n\r", argv[0]);
          fprintf(stderr, "%s send reply ok\n\r", argv[0]);
        } else
          fprintf(stderr, "%s term format error \n\r", argv[0]);

        erl_free_term(emsg.from); erl_free_term(emsg.msg);
        erl_free_term(fromp); erl_free_term(tuplep);
        erl_free_term(fnp); erl_free_term(argp);
        erl_free_term(resp);
      }
      break;
    default:
      fprintf(stderr, "%s something wrong! :(\n\r", argv[0]);
      loop = 0;
      break;
    }

  } /* while */
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
