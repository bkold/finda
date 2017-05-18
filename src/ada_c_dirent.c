#include <sys/types.h>
#include <dirent.h>

char *c_dirent_name (struct dirent *ep) {
	return ep->d_name;
}

long long int c_dirent_number (struct dirent *ep) {
	return ep->d_ino;
}

int c_dirent_mode (struct dirent *ep) {
	return ep->d_type;
}