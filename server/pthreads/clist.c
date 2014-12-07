#include <stdlib.h>

#include "clist.h"

struct _CList {
	uint64_t data;
	void *next;
};

CList *_clist_insert(CList *list, uint64_t data)
{
	CList *node = malloc(sizeof(*node));
	node->data = data;

	if (list == NULL) {
		node->next = node;
		return node;
	} else {
		node->next = list->next;
		list->next = node;
		return list;
	}
}

CList *clist_next(CList *list)
{
	return list->next;
}

uint64_t _clist_data(CList *list)
{
	return list->data;
}
