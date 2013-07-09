#include <stdlib.h>

#include "clist.h"

struct _CList {
	uint64_t data;
	void *next;
};

CList *_clist_insert(CList *list, uint64_t data)
{
	CList *node = malloc(sizeof(*node));
	if (list == NULL)
		list = node;
	node->next = list->next;
	list->next = node;
	node->data = data;
	return list;
}

CList *clist_next(CList *list)
{
	return list->next;
}

uint64_t _clist_data(CList *list)
{
	return list->data;
}
