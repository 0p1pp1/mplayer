#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>

#include <stdio.h>

int
connect2Server(char *host, int port) {
	int socket_server_fd;
	struct sockaddr_in server_address;
	printf(">>>> connect2Server [%s@%d]\n", host, port );
	socket_server_fd = socket(AF_INET, SOCK_STREAM, 0);
	if( socket_server_fd==-1 ) {
		perror("Failed to create socket");
		exit(1);
	}

	if( isalpha(host[0]) ) {
		struct hostent *hp;
		if( (hp=gethostbyname( host ))==NULL ) {
			printf("Unknown host: %s\n", host);
			exit(1);
		}
		memcpy( &server_address.sin_addr.s_addr, hp->h_addr, hp->h_length );
	} else {
		inet_pton(AF_INET, host, &server_address.sin_addr);
	}
	server_address.sin_family=AF_INET;
	server_address.sin_port=htons(port);

	if( connect( socket_server_fd, (struct sockaddr*)&server_address, sizeof(server_address) )==-1 ) {
		perror("Failed to connect to server");
		close(socket_Stream_fd);
		exit(1);
	}
	return socket_server_fd;
}

