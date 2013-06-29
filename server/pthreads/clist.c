#include <stdlib.h>

#include "clist.h"

struct _CList {
	int data;
	void *next;
};

CList *clist_insert(CList *list, int data)
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

int clist_data(CList *list)
{
	return list->data;
}
