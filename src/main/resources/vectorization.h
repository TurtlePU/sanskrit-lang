#if defined(__aarch64__) // ARM64 architecture
#include <arm_neon.h>
#define VECTOR_TYPE int32x4_t
#define VECTOR_LOAD(v) vld1q_s32(v)
#define VECTOR_STORE(v, p) vst1q_s32(p, v)
#define VECTOR_ADD(a, b) vaddq_s32(a, b)
#define VECTOR_MUL(a, b) vmulq_s32(a, b)

#elif defined(__x86_64__) // x86_64 architecture
#include <immintrin.h>
#define VECTOR_TYPE __m128i
#define VECTOR_LOAD(v) _mm_loadu_si128((__m128i*)(v))
#define VECTOR_STORE(v, p) _mm_storeu_si128((__m128i*)(p))
#define VECTOR_ADD(a, b) _mm_add_epi32(a, b)
#define VECTOR_MUL(a, b) _mm_mullo_epi32(a, b)
#endif

int* vectorized_add(const int *a, const int *b, size_t size) {
    size_t i;

    int* result = (int*)malloc(size * sizeof(int));

    for (i = 0; i < size - (size % 4); i += 4) {
        VECTOR_TYPE va = VECTOR_LOAD(&a[i]);
        VECTOR_TYPE vb = VECTOR_LOAD(&b[i]);
        VECTOR_TYPE vadd = VECTOR_ADD(va, vb);
        VECTOR_STORE(vadd, &result[i]);
    }

    for (; i < size; ++i) {
        result[i] = a[i] + b[i];
    }
}

int* vectorized_mul(const int *a, const int *b, size_t size) {
    size_t i;

    int* result = (int*)malloc(size * sizeof(int));

    for (i = 0; i < size - (size % 4); i += 4) {
        VECTOR_TYPE va = VECTOR_LOAD(&a[i]);
        VECTOR_TYPE vb = VECTOR_LOAD(&b[i]);
        VECTOR_TYPE vmul = VECTOR_MUL(va, vb);
        VECTOR_STORE(vmul, &result[i]);
    }

    for (; i < size; ++i) {
        result[i] = a[i] * b[i];
    }
}
