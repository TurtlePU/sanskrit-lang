#include "klib/khash.h"
#include <stdlib.h>
#include <stdio.h>

#define MEMO_CACHE_SIZE 128

// lru node struct name with value of type TYPE
#define LRU_NODE(TYPE) _LRUNode_##TYPE

// definition of lru node struct with value type TYPE
#define DEFINE_LRU_NODE(TYPE) \
    typedef struct LRU_NODE(TYPE) { \
        struct LRU_NODE(TYPE)* prev; \
        struct LRU_NODE(TYPE)* next; \
        int arg; \
        TYPE val; \
    } LRU_NODE(TYPE);

// lru struct name with values of type TYPE
#define LRU(TYPE) _LRU_##TYPE

// definition of lru struct with value type TYPE
#define DEFINE_LRU(TYPE) \
    typedef struct LRU(TYPE) { \
        LRU_NODE(TYPE)* head; \
        LRU_NODE(TYPE)* tail; \
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
        LRU(TYPE)* lru; \
        khash_t(memo_##TYPE)* ht; \
    } MEMO_CACHE(TYPE); \
    \
    MEMO_CACHE(TYPE)* create_memo_cache_##TYPE() { \
        MEMO_CACHE(TYPE)* memo = (MEMO_CACHE(TYPE)*)malloc(sizeof(MEMO_CACHE(TYPE))); \
        memo->lru = (LRU(TYPE)*)malloc(sizeof(LRU(TYPE))); \
        memo->lru->head = NULL; \
        memo->lru->tail = NULL; \
        memo->ht = kh_init(memo_##TYPE); \
        return memo; \
    } \
    \
    void add_memo_node_##TYPE(MEMO_CACHE(TYPE)* cache, LRU_NODE(TYPE)* node) { \
        node->next = cache->lru->head; \
        node->prev = NULL; \
        if (cache->lru->head != NULL) { \
            cache->lru->head->prev = node; \
        } \
        cache->lru->head = node; \
        if (cache->lru->tail == NULL) { \
            cache->lru->tail = node; \
        } \
    } \
    \
    void remove_memo_node_##TYPE(MEMO_CACHE(TYPE)* cache, LRU_NODE(TYPE)* node) { \
        if (node->prev != NULL) { \
            node->prev->next = node->next; \
        } else { \
            cache->lru->head = node->next; \
        } \
        if (node->next != NULL) { \
            node->next->prev = node->prev; \
        } else { \
            cache->lru->tail = node->prev; \
        } \
    } \
    \
    void move_memo_node_to_front_##TYPE(MEMO_CACHE(TYPE)* cache, LRU_NODE(TYPE)* node) { \
        remove_memo_node_##TYPE(cache, node); \
        add_memo_node_##TYPE(cache, node); \
    } \
    \
    TYPE* memo_cache_get_##TYPE(MEMO_CACHE(TYPE)* cache, int arg) { \
        khint_t k = kh_get(memo_##TYPE, cache->ht, arg); \
        if (k == kh_end(cache->ht)) { \
            return NULL; \
        } \
        LRU_NODE(TYPE)* node = kh_value(cache->ht, k); \
        move_memo_node_to_front_##TYPE(cache, node); \
        return &node->val; \
    } \
    \
    void memo_cache_set_##TYPE(MEMO_CACHE(TYPE)* cache, int arg, TYPE val) { \
        khint_t k = kh_get(memo_##TYPE, cache->ht, arg); \
        if (k != kh_end(cache->ht)) { /* should never reach this, but just in case */ \
            LRU_NODE(TYPE)* node = kh_value(cache->ht, k); \
            node->val = val; \
            move_memo_node_to_front_##TYPE(cache, node); \
        } else { \
            if (kh_size(cache->ht) == MEMO_CACHE_SIZE) { \
                LRU_NODE(TYPE)* tail_node = cache->lru->tail; \
                k = kh_get(memo_##TYPE, cache->ht, tail_node->arg); \
                kh_del(memo_##TYPE, cache->ht, k); \
                remove_memo_node_##TYPE(cache, tail_node); \
            } \
            \
            LRU_NODE(TYPE)* new_node = (LRU_NODE(TYPE)*)malloc(sizeof(LRU_NODE(TYPE))); \
            new_node->arg = arg; \
            new_node->val = val; \
            add_memo_node_##TYPE(cache, new_node); \
            int absent; \
            k = kh_put(memo_##TYPE, cache->ht, arg, &absent); \
            kh_value(cache->ht, k) = new_node; \
        } \
    } \
    \
    void memo_cache_free_##TYPE(MEMO_CACHE(TYPE)* cache) { \
        for (LRU_NODE(TYPE)* node = cache->lru->head; node != NULL; node = node->next) { \
            remove_memo_node_##TYPE(cache, node); \
            int k = kh_get(memo_##TYPE, cache->ht, node->arg); \
            kh_del(memo_##TYPE, cache->ht, k); \
            free(node); \
        } \
        free(cache->lru); \
        kh_destroy(memo_##TYPE, cache->ht); \
        free(cache); \
    } \
    \
    TYPE memoize_##TYPE(MEMO_CACHE(TYPE)* cache, TYPE (*impl)(int, void*), int arg, void* env) { \
        TYPE* cache_result = NULL; \
        if (cache != NULL) { \
            TYPE* cache_result = memo_cache_get_##TYPE(cache, arg); \
        } \
        if (cache_result != NULL) { \
            return *cache_result; \
        } \
        TYPE res = impl(arg, env); \
        memo_cache_set_##TYPE(cache, arg, res); \
        return res; \
    }
