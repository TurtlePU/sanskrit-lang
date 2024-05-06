#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
   int (*impl)(int, void*);
   void* env;
} int_int_closure;

typedef struct {
   int_int_closure (*impl)(int_int_closure, void*);
   void* env;
} int_int_closure_int_int_closure_closure;

typedef struct {
int_int_closure f;
int x;
} env_953782652;

int func_953782652(int x, void* data) {
env_953782652* env = (env_953782652*)data;
env->x = x;
int fRes = (env->f.impl(x, env->f.env));
return (env->f.impl(fRes, env->f.env));
}

int_int_closure create_func_953782652(void* prevEnv, size_t prevEnvSize) {
env_953782652* env = malloc(sizeof(env_953782652));
if (prevEnv != NULL) {
memcpy(env, prevEnv, prevEnvSize);
}
return (int_int_closure){.impl=func_953782652, .env=env};
}

typedef struct {
int_int_closure f;
} env_1263448690;

int_int_closure func_1263448690(int_int_closure f, void* data) {
env_1263448690* env = (env_1263448690*)data;
env->f = f;
return create_func_953782652(env, sizeof(*env));
}

int_int_closure_int_int_closure_closure create_func_1263448690(void* prevEnv, size_t prevEnvSize) {
env_1263448690* env = malloc(sizeof(env_1263448690));
if (prevEnv != NULL) {
memcpy(env, prevEnv, prevEnvSize);
}
return (int_int_closure_int_int_closure_closure){.impl=func_1263448690, .env=env};
}

typedef struct {
int_int_closure_int_int_closure_closure dup;
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
int_int_closure_int_int_closure_closure dup = create_func_1263448690(NULL, 0);
int_int_closure f = create_func_1763225131(NULL, 0);
int_int_closure fDup = (dup.impl(f, dup.env));
int x = 5;
int main = (fDup.impl(x, fDup.env));
printf("%d\n", main);
free(f.env);
free(dup.env);
free(fDup.env);
return 0;
}
