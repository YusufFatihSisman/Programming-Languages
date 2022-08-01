#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "helper.h"

Table *t;

void createTable(){
	t = (Table*)malloc(sizeof(Table));
	t->size = 0;
	t->capacity = 2;
	t->table = (Member*)malloc(sizeof(Member) * t->capacity);
}

int isExist(char *id){
	int i = 0;
	for(i = 0; i < t->size; i++){
		if(strcmp(t->table[i].id, id) == 0){
			return i;
		}
	}
	return -1;
}

void tableRealloc(){
	t->capacity *= 2;
	t->table = (Member*)realloc(t->table, sizeof(Member) * t->capacity);
}

void addToTable(char *id, int val){
	int i = isExist(id);
	if(i != -1){
		t->table[i].val = val;
		return;
	}
	if(t->size == t->capacity){
		tableRealloc(t);
	}
	strcpy(t->table[t->size].id, id);
	t->table[t->size].val = val;
	t->size += 1; 
}

int getIdFromTable(char *id){
	int i = isExist(id);
	if(i == -1){
		printf("ERROR : Identifier %s has not been defined.\n", id);
		freeTable();
		exit(-1);		
	}	
	return t->table[i].val;
}

void freeTable(){
	if(t != NULL){
		if(t->table != NULL){
			free(t->table);
		}
		free(t);
	}
}

List* createList(){
	List *l = (List*)malloc(sizeof(List));
	l->size = 0;
	l->capacity = 2;
	l->list = (int*)malloc(sizeof(int) * l->capacity);
	return l;
}

List* appendList(List *l, int val){
	List *newL = createList();
	newL->list[newL->size] = val;
	newL->size = 1;
	if(l != NULL){
		newL->capacity = l->capacity * 2;
		newL->list = (int*)realloc(newL->list, sizeof(int) * newL->capacity);
		int i = 0;
		for(i = 0; i < l->size; i++){
			newL->list[i+1] = l->list[i];
		}
		newL->size += l->size;
		free(l);
	}
	return newL;
}

List* addToList(List *l, int val){
	List *newL = createList();
	if(l != NULL){
		newL->capacity = l->capacity * 2;
		newL->list = (int*)realloc(newL->list, sizeof(int) * newL->capacity);
		int i = 0;
		for(i = 0; i < l->size; i++){
			newL->list[i] = l->list[i];
		}
		newL->size = l->size;
	}
	free(l);
	newL->list[newL->size] = val;
	newL->size += 1;
	return newL;
}

List* concatList(List *l1, List *l2){
	if(l1 == NULL){
		return l2;
	}else if(l2 == NULL){
		return l1;
	}
	List *newL = createList();
	newL->capacity = l1->capacity + l2->capacity;
	newL->list = (int*)realloc(newL->list, sizeof(int) * newL->capacity);
	int i = 0;
	for(i = 0; i < l1->size; i++){
		newL->list[i] = l1->list[i];
	}
	int j = l1->size;
	for(i = 0; i < l2->size; i++){
		newL->list[j++] = l2->list[i];
	}
	newL->size = j;
	free(l1);
	free(l2);
	return newL;
}

void printList(List *l){
	printf("(");
	if(l != NULL){
		int i = 0;
		for(i = 0; i < l->size; i++){
			printf("%d", l->list[i]);
			if(i != l->size - 1){
				printf(" ");
			}
		}
	}
	printf(")\n");
}

void freeList(List *l){
	if(l != NULL){
		if(l->list != NULL){
			free(l->list);
		}
		free(l);
	}
}




