#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
   int (*impl)(int, void*);
   void* env;
} int_int_closure;

typedef struct {
int x;
} env_1763225131;

int func_1763225131(int x, void* data) {
env_1763225131* env = (env_1763225131*)data;
env->x = x;
return env->x * env->x;
}

int_int_closure create_func_1763225131(void* prevEnv, size_t prevEnvSize) {
env_1763225131* env = malloc(sizeof(env_1763225131));
if (prevEnv != NULL) {
memcpy(env, prevEnv, prevEnvSize);
}
return (int_int_closure){.impl=func_1763225131, .env=env};
}

int main() {
int_int_closure f = create_func_1763225131(NULL, 0);
int x = 5;
int main = (f.impl(x, f.env));
printf("%d\n", main);
free(f.env);
return 0;
}
