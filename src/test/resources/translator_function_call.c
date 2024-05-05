#include <stdio.h>
#include <stdlib.h>

typedef struct {
   int (*impl)(int, void*);
   void* env;
} int_int_closure;

typedef struct {
int x;
} env_1076984342;

int func_1076984342(int x, void* data) {
env_1076984342* env = (env_1076984342*)data;
env->x = x;
return env->x * env->x;
}

int_int_closure create_func_1076984342() {
env_1076984342* env = malloc(sizeof(env_1076984342));

return (int_int_closure){.impl=func_1076984342, .env=env};
}

int main() {
int_int_closure f = create_func_1076984342();
int x = 5;
int main = (f.impl(x, f.env));
printf("%d\n", main);
free(f.env);
return 0;
}
