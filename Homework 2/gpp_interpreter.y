%{
	#include<stdio.h>
	#include "helper.h"

	int yylex(void);
	int yyerror(char *str);

	void yyrestart (FILE *input_file);
	extern FILE *yyin;
	
	int dontPrint = 0;
%}

%union{
    int value;
    void *values;
    char id[30];
}

%start INPUT

%token OP_PLUS
%token OP_MINUS
%token OP_DIV
%token OP_MULT
%token OP_OP
%token OP_CP
%token OP_DBLMULT
%token OP_OC
%token OP_CC
%token OP_COMMA
%token KW_AND
%token KW_OR
%token KW_NOT
%token KW_EQUAL
%token KW_LESS
%token KW_NIL
%token KW_LIST
%token KW_APPEND
%token KW_CONCAT
%token KW_SET
%token KW_DEFFUN
%token KW_FOR
%token KW_IF
%token KW_EXIT
%token KW_LOAD
%token KW_DISP
%token KW_TRUE
%token KW_FALSE
%token COMMENT

%token <value> VALUE
%token <id> IDENTIFIER

%type <value> INPUT
%type <value> EXPI
%type <value> EXPB
%type <values> EXPLISTI
%type <values> LISTVALUE
%type <values> VALUES

%% 
INPUT : 
	EXPI {if(dontPrint == 0)printf("Syntax OK.\nResult: %d\n", $1); dontPrint = 0;}
	| INPUT EXPI {if(dontPrint == 0)printf("Syntax OK.\nResult: %d\n", $2); dontPrint = 0;}
	| EXPLISTI { printf("Syntax OK.\nResult: "); printList($1); }
	| INPUT EXPLISTI { printf("Syntax OK.\nResult: "); printList($2); }
	| EXPB { printf("Syntax OK.\nResult: %s\n", $1 == 1 ? "T" : "NIL"); }
	| INPUT EXPB { printf("Syntax OK.\nResult: %s\n", $2 == 1 ? "T" : "NIL"); }
	| COMMENT { }
	| INPUT COMMENT { }
	| OP_OP KW_EXIT OP_CP { printf("Syntax OK.\nProgram terminated.\n"); freeTable(); return(1); }
	| INPUT OP_OP KW_EXIT OP_CP { printf("Syntax OK.\nProgram terminated.\n"); freeTable(); return(1); }
	;

EXPI :
	OP_OP OP_PLUS EXPI EXPI OP_CP {$$ = $3 + $4;}
	| OP_OP OP_MINUS EXPI EXPI OP_CP {$$ = $3 - $4;}	
	| OP_OP OP_MULT EXPI EXPI OP_CP {$$ = $3 * $4;}
	| OP_OP OP_DIV EXPI EXPI OP_CP {$$ = $3 / $4;}
	| OP_OP KW_SET IDENTIFIER EXPI OP_CP {$$ = $4; addToTable($3, $4);}
	| OP_OP KW_IF EXPB EXPI OP_CP {$$ = $3 == 1 ? $4 : 0; }
	| OP_OP KW_IF EXPB EXPI EXPI OP_CP {$$ = $3 == 1 ? $4 : $5; }
	| OP_OP KW_IF EXPB EXPLISTI OP_CP {dontPrint = 1; $$ = $3; printf("Syntax OK.\nResult: "); $3 == 1 ? printList($4) : printList(NULL);}
	| OP_OP KW_IF EXPB EXPLISTI EXPLISTI OP_CP {dontPrint = 1; $$ = $3; printf("Syntax OK.\nResult: "); $3 == 1 ? printList($4) : printList($5);}
	| IDENTIFIER {$$ = getIdFromTable($1);}
	| VALUE {$$ = $1;}
	;

EXPB :
	OP_OP KW_AND EXPB EXPB OP_CP {$$ = $3 && $4;}
	| OP_OP KW_OR EXPB EXPB OP_CP {$$ = $3 || $4;}
	| OP_OP KW_NOT EXPB OP_CP {$$ = !$3;}
	| OP_OP KW_EQUAL EXPB EXPB OP_CP {$$ = $3==$4;}
	| OP_OP KW_EQUAL EXPI EXPI OP_CP {$$ = $3==$4;}
	| OP_OP KW_LESS EXPI EXPI OP_CP {$$ = $3 < $4;}
	| KW_TRUE {$$ = 1;}
	| KW_FALSE {$$ = 0;}
	;

EXPLISTI :
	OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP {$$ = concatList($3, $4);}
	| OP_OP KW_APPEND EXPI EXPLISTI OP_CP {$$ = appendList($4, $3);}
	| LISTVALUE {$$ = $1;}
	;

LISTVALUE :
	OP_OP KW_LIST VALUES OP_CP {$$ = $3;}
	| OP_OP KW_LIST OP_CP {$$ = createList();}
	| KW_NIL {$$ = NULL;}
	;

VALUES :
	VALUES VALUE {$$ = addToList($1, $2);}
	| VALUE {$$ = addToList(NULL, $1);}
	;

%%

int yyerror(char *str){
	fprintf(stderr, "SYNTAX_ERROR Expression not recognized\n");
	return 0;
}

int main(int argc, char **argv){
	createTable();
	//FILE *out = fopen("outputfile.txt", "w");
	if(argc > 1){
		yyin = fopen(argv[1], "r");
		yyrestart(yyin);
		if(yyparse() == 1){
			fclose(yyin);
			return 0;
		}
		fclose(yyin);
	}
	yyin = stdin;
	yyrestart(yyin);
	while(1) {
		if(yyparse() == 1) {
			return 0;
		}
	}
	freeTable();
	return 0;
}