#ifndef FILE_IO_H
#define FILE_IO_H

// Imported file handling
int fileopenw(char *fname);

int fileopenr(char *fname);

int fileseek(int stream, int offset, int origin);

int filesize(int stream);

int fileread(void *ptr, int size, int count, int stream);

int filewrite(void *ptr, int size, int count, int stream);

int fileclose(int stream);

int filetruncate(int stream, int len);

// macro helpers
int read1 (int stream, void *ptr, int count);

int write1 (int stream, void *ptr, int count);

#endif
