#ifndef HELPER_H_INCLUDED
#define HELPER_H_INCLUDED

typedef struct Member{
	int val;
	char id[30];
}Member;

typedef struct Table{
	Member *table;
	int size;
	int capacity;
}Table;

typedef struct List{
	int *list;
	int size;
	int capacity;
}List;

void createTable();
int isExist(char *id);
void tableRealloc();
void addToTable(char *id, int val);
int getIdFromTable(char *id);
void freeTable();
List* createList();
List* addToList(List *l, int val);
List* appendList(List *l, int val);
List* concatList(List *l1, List *l2);
void printList(List *l);
void freeList(List *l);

#endif
