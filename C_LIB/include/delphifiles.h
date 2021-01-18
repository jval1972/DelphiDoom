#ifndef _DELPHIFILES_H_
#define _DELPHIFILES_H_

// Imported file handling

int fileopenw(char *fname);

int fileopena(char *fname);

int fileopenr(char *fname);

int fileseek(int stream, int offset, int origin);

int filesize(int stream);

int filetell(int stream);

__int64 filetell64(int stream);

int fileread(void *ptr, int size, int count, int stream);

int filewrite(void *ptr, int size, int count, int stream);

int fileclose(int stream);

int filetruncate(int stream, int len);

int filerename(char *fin, char *fout);

int fileerror();

int fileeof(int stream);

int filepos(int stream);

int fileseek64(int stream, __int64 offset, int origin);

#endif
