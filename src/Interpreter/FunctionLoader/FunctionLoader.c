#include <dlfcn.h>
#include <stdio.h>
#include <math.h>

void *libc = NULL;
void *getLibC()
{
    if (libc == NULL)
        libc = dlopen("libc", 0);
    return libc;
}

void *getLibCFunction(char *func)
{
    return dlsym(getLibC(), func);
}

// int main()
// {
//     void (*func)(char *) = getLibCFunction("puts");
//     func("yao");
//     return 0;
// }