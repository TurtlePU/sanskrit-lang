#include "memo_cache.h"
#include "vectorization.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

DEFINE_MEMO_CACHE(int)

typedef struct {
   int (*impl)(int, void*);
   void* env;
   MEMO_CACHE(int)* memo;
} int_int_closure;

typedef struct {
   int_int_closure (*impl)(int_int_closure, void*);
   void* env;
} int_int_closure_int_int_closure_closure;

typedef struct {
int_int_closure f;
int x;
} env_1186078714;

int func_1186078714(int x, void* data) {
env_1186078714* env = (env_1186078714*)data;
env->x = x;
int fRes = memoize_int(env->f.memo, env->f.impl, x, env->f.env);
return memoize_int(env->f.memo, env->f.impl, fRes, env->f.env);
}

int_int_closure create_func_1186078714(void* prevEnv, size_t prevEnvSize) {
env_1186078714* env = malloc(sizeof(env_1186078714));
if (prevEnv != NULL) {
memcpy(env, prevEnv, prevEnvSize);
}
return (int_int_closure){.impl=func_1186078714, .env=env, .memo=create_memo_cache_int()};
}

typedef struct {
int_int_closure f;
} env_2110837104;

int_int_closure func_2110837104(int_int_closure f, void* data) {
env_2110837104* env = (env_2110837104*)data;
env->f = f;
return create_func_1186078714(env, sizeof(*env));
}

int_int_closure_int_int_closure_closure create_func_2110837104(void* prevEnv, size_t prevEnvSize) {
env_2110837104* env = malloc(sizeof(env_2110837104));
if (prevEnv != NULL) {
memcpy(env, prevEnv, prevEnvSize);
}
return (int_int_closure_int_int_closure_closure){.impl=func_2110837104, .env=env};
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
return (int_int_closure){.impl=func_1763225131, .env=env, .memo=create_memo_cache_int()};
}

int main() {
int_int_closure_int_int_closure_closure dup = create_func_2110837104(NULL, 0);
int_int_closure f = create_func_1763225131(NULL, 0);
int_int_closure fDup = dup.impl(f, dup.env);
int x = 5;
int main = memoize_int(fDup.memo, fDup.impl, x, fDup.env);
printf("%d\n", main);
free(f.env);
memo_cache_free_int(f.memo);
free(dup.env);
free(fDup.env);
memo_cache_free_int(fDup.memo);
return 0;
}
