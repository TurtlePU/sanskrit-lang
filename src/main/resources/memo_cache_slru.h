#include "klib/khash.h"
#include <stdlib.h>
#include <stdio.h>

#define MEMO_CACHE_SIZE_PROTECTED 128
#define MEMO_CACHE_SIZE_PROBATION 128

// lru node struct name with value of type TYPE
#define LRU_NODE(TYPE) _LRUNode_##TYPE

// definition of lru node struct with value type TYPE
#define DEFINE_LRU_NODE(TYPE) \
    typedef struct LRU_NODE(TYPE) { \
        struct LRU_NODE(TYPE)* prev; \
        struct LRU_NODE(TYPE)* next; \
        int arg; \
        TYPE val; \
        int is_protected; \
    } LRU_NODE(TYPE);

#define LRU(TYPE) _LRU_##TYPE

#define DEFINE_LRU(TYPE) \
    typedef struct { \
        LRU_NODE(TYPE)* head; \
        LRU_NODE(TYPE)* tail; \
        int size; \
    } LRU(TYPE);

#define MEMO_CACHE(TYPE) _MemoCache_##TYPE

#define DEFINE_MEMO_CACHE(TYPE) \
    DEFINE_LRU_NODE(TYPE) \
    \
    DEFINE_LRU(TYPE) \
    \
    KHASH_MAP_INIT_INT(memo_##TYPE, LRU_NODE(TYPE)*) \
    \
    typedef struct MEMO_CACHE(TYPE) { \
        LRU(TYPE)* protected; \
        LRU(TYPE)* probation; \
        khash_t(memo_##TYPE)* ht; \
    } MEMO_CACHE(TYPE); \
    \
    MEMO_CACHE(TYPE)* create_memo_cache_##TYPE() { \
        MEMO_CACHE(TYPE)* memo = (MEMO_CACHE(TYPE)*)malloc(sizeof(MEMO_CACHE(TYPE))); \
        memo->protected = (LRU(TYPE)*)malloc(sizeof(LRU(TYPE))); \
        memo->probation = (LRU(TYPE)*)malloc(sizeof(LRU(TYPE))); \
        memo->protected->head = memo->protected->tail = NULL; \
        memo->protected->size = 0; \
        memo->probation->head = memo->probation->tail = NULL; \
        memo->probation->size = 0; \
        memo->ht = kh_init(memo_##TYPE); \
        return memo; \
    } \
    \
    void add_memo_node_##TYPE(LRU(TYPE)* list, LRU_NODE(TYPE)* node) { \
        node->next = list->head; \
        node->prev = NULL; \
        if (list->head != NULL) { \
            list->head->prev = node; \
        } \
        list->head = node; \
        if (list->tail == NULL) { \
            list->tail = node; \
        } \
        ++list->size; \
    } \
    \
    void remove_memo_node_##TYPE(LRU(TYPE)* list, LRU_NODE(TYPE)* node) { \
        if (node->prev != NULL) { \
            node->prev->next = node->next; \
        } else { \
            list->head = node->next; \
        } \
        if (node->next != NULL) { \
            node->next->prev = node->prev; \
        } else { \
            list->tail = node->prev; \
        } \
        --list->size; \
    } \
    \
    void promote_memo_node_##TYPE(MEMO_CACHE(TYPE)* cache, LRU_NODE(TYPE)* node) { \
        if (node->is_protected) { \
            remove_memo_node_##TYPE(cache->protected, node); \
            add_memo_node_##TYPE(cache->probation, node); \
        } else { \
            remove_memo_node_##TYPE(cache->protected, node); \
            if (cache->protected->size >= MEMO_CACHE_SIZE_PROTECTED) { \
                LRU_NODE(TYPE)* node_to_evict = cache->protected->tail; \
                remove_memo_node_##TYPE(cache->protected, node_to_evict); \
                free(node_to_evict); \
            } \
            node->is_protected = 1; \
            add_memo_node_##TYPE(cache->protected, node); \
        } \
    } \
    \
    TYPE* memo_cache_get_##TYPE(MEMO_CACHE(TYPE)* cache, int arg) { \
        khint_t k = kh_get(memo_##TYPE, cache->ht, arg); \
        if (k == kh_end(cache->ht)) { \
            return NULL; \
        } \
        LRU_NODE(TYPE)* node = kh_value(cache->ht, k); \
        promote_memo_node_##TYPE(cache, node); \
        return &(node->val); \
    } \
    \
    void memo_cache_set_##TYPE(MEMO_CACHE(TYPE)* cache, int arg, TYPE val) { \
        khint_t k = kh_get(memo_##TYPE, cache->ht, arg); \
        if (k != kh_end(cache->ht)) { /* should never reach this, but just in case */ \
            LRU_NODE(TYPE)* node = kh_value(cache->ht, k); \
            node->val = val; \
            promote_memo_node_##TYPE(cache, node); \
        } else { \
            if (cache->probation->size >= MEMO_CACHE_SIZE_PROBATION) { \
                LRU_NODE(TYPE)* node_to_evict = cache->probation->tail; \
                k = kh_get(memo_##TYPE, cache->ht, node_to_evict->arg); \
                kh_del(memo_##TYPE, cache->ht, k); \
                remove_memo_node_##TYPE(cache->probation, node_to_evict); \
                free(node_to_evict); \
            } \
            \
            LRU_NODE(TYPE)* new_node = (LRU_NODE(TYPE)*)malloc(sizeof(LRU_NODE(TYPE))); \
            new_node->arg = arg; \
            new_node->val = val; \
            new_node->is_protected = 0; \
            add_memo_node_##TYPE(cache->probation, new_node); \
            int absent; \
            k = kh_put(memo_##TYPE, cache->ht, arg, &absent); \
            kh_value(cache->ht, k) = new_node; \
        } \
    } \
    \
    void memo_cache_free_##TYPE(MEMO_CACHE(TYPE)* cache) { \
        for (LRU_NODE(TYPE)* node = cache->probation->head; node != NULL; node = node->next) { \
            remove_memo_node_##TYPE(cache->probation, node); \
            int k = kh_get(memo_##TYPE, cache->ht, node->arg); \
            kh_del(memo_##TYPE, cache->ht, k); \
            free(node); \
        } \
        for (LRU_NODE(TYPE)* node = cache->protected->head; node != NULL; node = node->next) { \
            remove_memo_node_##TYPE(cache->protected, node); \
            int k = kh_get(memo_##TYPE, cache->ht, node->arg); \
            kh_del(memo_##TYPE, cache->ht, k); \
            free(node); \
        } \
        free(cache->protected); \
        free(cache->probation); \
        kh_destroy(memo_##TYPE, cache->ht); \
        free(cache); \
    } \
    \
    TYPE inline __attribute__((always_inline)) memoize_##TYPE(MEMO_CACHE(TYPE)* cache, TYPE (*impl)(int, void*, MEMO_CACHE(TYPE)*), int arg, void* env) { \
        TYPE* cache_result = NULL; \
        if (cache != NULL) { \
            cache_result = memo_cache_get_##TYPE(cache, arg); \
        } \
        if (cache_result != NULL) { \
            return *cache_result; \
        } \
        TYPE res = impl(arg, env, cache); \
        memo_cache_set_##TYPE(cache, arg, res); \
        return res; \
    }
