#include <arpa/inet.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>
int main(int argc, char **argv) {
  int port = atoi(argv[1]);
  int fd = socket(AF_INET, SOCK_STREAM, 0);
  if (fd < 0) { perror("socket"); return 2; }
  struct sockaddr_in a = {0}; a.sin_family = AF_INET; a.sin_port = htons(port);
  a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  if (connect(fd, (struct sockaddr*)&a, sizeof a) < 0) { perror("connect"); return 3; }
  const char *m = "ping";
  if (send(fd, m, 4, 0) != 4) { perror("send"); return 4; }
  char buf[16] = {0};
  int n = recv(fd, buf, sizeof buf - 1, 0);
  if (n < 0) { perror("recv"); return 5; }
  buf[n] = 0;
  printf("p2-sock:%s\n", buf);
  close(fd);
  return 0;
}
