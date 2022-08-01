%option noyywrap
%{
	#include<stdio.h>

	#define NEWL 0
	#define VALUE 1
	#define AND 2
	#define OR 3
	#define NOT 4
	#define EQUAL 5
	#define LESS 6
	#define NIL 7
	#define LIST 8
	#define APPEND 9
	#define CONCAT 10
	#define SET 11
	#define DEFFUN 12
	#define FOR 13
	#define IF 14
	#define EXIT 15
	#define LOAD 16
	#define DISP 17
	#define TRUE 18
	#define FALSE 19
	#define PLUS 20
	#define MINUS 21
	#define DIV 22
	#define MULT 23
	#define OP 24
	#define CP 25
	#define DMULT 26
	#define COMMA 27
	#define QUOTE 28
	#define COMMENT 29
	#define IDENT 30
	#define ERROR 31
	#define EOFILE 32
	#define OTHER 33

	int quoteCounter = 0;
	int newL = 0;
	int other = 0;
%}

value [0-9]|([1-9]([0-9])+)
real [0-9]"."([0-9]+)|([1-9]([0-9])+)"."([0-9]+)
identifier [a-zA-Z]|([a-zA-Z]([a-zA-Z]|[0-9])+)

errorValue 0([0-9]+)
errorReal 0([0-9]+).([0-9]+)
errorIdent ([0-9]+)([a-zA-Z0-9]+)
comment ;;(.)*


%%
{value}		      return VALUE;
{real}		      return VALUE;
"and"|"AND"	      return AND;
"or"|"OR"	      return OR;
"not"|"NOT"	      return NOT;
"equal"|"EQUAL"	      return EQUAL;
"less"|"LESS"	      return LESS;
"nil"|"NIL"           return NIL;
"list"|"LIST"	      return LIST;
"append"|"APPEND"     return APPEND;
"concat"|"CONCAT"     return CONCAT;
"set"|"SET"           return SET;
"deffun"|"DEFFUN"     return DEFFUN;
"for"|"FOR"           return FOR;
"if"|"IF"             return IF;
"exit"|"EXIT"         return EXIT;
"load"|"LOAD"         return LOAD;
"disp"|"DISP"         return DISP;
"true"|"TRUE"         return TRUE;
"false"|"FALSE"       return FALSE;
"+"		      return PLUS;	
"-"		      return MINUS;
"/"		      return DIV;
"*"		      return MULT;
"("		      return OP;
")"		      return CP;
"**"		      return DMULT;
","		      return COMMA;
\"                    return QUOTE;
{errorValue}          return ERROR;
{errorReal}	      return ERROR;
{errorIdent}	      return ERROR;
{comment}	      return COMMENT;
" "		      return OTHER;
{identifier}	      return IDENT;
<<EOF>>		      return EOFILE;
\r		      return OTHER;
\t		      return OTHER;
\tab		      return OTHER;
\n                    return NEWL;
[.+]		      return ERROR;
%%

int main(int argc, char *argv[])
{
	int c;
	FILE *fileOut = fopen("parsed_cpp.txt", "w");
	if(argv[1] != NULL){
		yyin = fopen(argv[1], "r");
		c = yylex();
		while(c != EOFILE){
			if(c == VALUE){
				fprintf(fileOut, "VALUE\n");
				printf("VALUE\n");
			}else if(c == AND){
				fprintf(fileOut, "KW_AND\n");
				printf("KW_AND\n");
			}else if(c == OR){
				fprintf(fileOut, "KW_OR\n");
				printf("KW_OR\n");
			}else if(c == NOT){
				fprintf(fileOut, "KW_NOT\n");
				printf("KW_NOT\n");
			}else if(c == EQUAL){
				fprintf(fileOut, "KW_EQUAL\n");
				printf("KW_EQUAL\n");
			}else if(c == LESS){
				fprintf(fileOut, "KW_LESS\n");
				printf("KW_LESS\n");
			}else if(c == NIL){
				fprintf(fileOut, "KW_NIL\n");
				printf("KW_NIL\n");
			}else if(c == LIST){
				fprintf(fileOut, "KW_LIST\n");
				printf("KW_LIST\n");
			}else if(c == APPEND){
				fprintf(fileOut, "KW_APPEND\n");
				printf("KW_APPEND\n");
			}else if(c == CONCAT){
				fprintf(fileOut, "KW_CONCAT\n");
				printf("KW_CONCAT\n");
			}else if(c == SET){
				fprintf(fileOut, "KW_SET\n");
				printf("KW_SET\n");
			}else if(c == DEFFUN){
				fprintf(fileOut, "KW_DEFFUN\n");
				printf("KW_DEFFUN\n");				
			}else if(c == FOR){
				fprintf(fileOut, "KW_FOR\n");
				printf("KW_FOR\n");
			}else if(c == IF){
				fprintf(fileOut, "KW_IF\n");
				printf("KW_IF\n");
			}else if(c == EXIT){
				fprintf(fileOut, "KW_EXIT\n");
				printf("KW_EXIT\n");
			}else if(c == LOAD){
				fprintf(fileOut, "KW_LOAD\n");
				printf("KW_LOAD\n");
			}else if(c == DISP){
				fprintf(fileOut, "KW_DISP\n");
				printf("KW_DISP\n");
			}else if(c == TRUE){
				fprintf(fileOut, "KW_TRUE\n");
				printf("KW_TRUE\n");
			}else if(c == FALSE){
				fprintf(fileOut, "KW_FALSE\n");
				printf("KW_FALSE\n");
			}else if(c == PLUS){
				fprintf(fileOut, "OP_PLUS\n");
				printf("OP_PLUS\n");
			}else if(c == MINUS){
				fprintf(fileOut, "OP_MINUS\n");
				printf("OP_MINUS\n");
			}else if(c == DIV){
				fprintf(fileOut, "OP_DIV\n");
				printf("OP_DIV\n");
			}else if(c == MULT){
				fprintf(fileOut, "OP_MULT\n");
				printf("OP_MULT\n");
			}else if(c == OP){
				fprintf(fileOut, "OP_OP\n");
				printf("OP_OP\n");
			}else if(c == CP){
				fprintf(fileOut, "OP_CP\n");
				printf("OP_CP\n");
			}else if(c == DMULT){
				fprintf(fileOut, "OP_DBLMULT\n");
				printf("OP_DBLMULT\n");
			}else if(c == COMMA){
				fprintf(fileOut, "OP_COMMA\n");
				printf("OP_COMMA\n");
			}else if(c == QUOTE){
				quoteCounter++;
				if(quoteCounter % 2 == 0){
					fprintf(fileOut, "OP_CC\n");
					printf("OP_CC\n");
				}else{
					fprintf(fileOut, "OP_OC\n");
					printf("OP_OC\n");
				}
			}else if(c == COMMENT){
				fprintf(fileOut, "COMMENT\n");
				printf("COMMENT\n");
			}else if(c == IDENT){
				fprintf(fileOut, "IDENTIFIER\n");
				printf("IDENTIFIER\n");
			}else if(c == ERROR){
				fprintf(fileOut, "SYNTAX_ERROR  %s can not be tokenized\n", yytext);
				printf("SYNTAX_ERROR  %s can not be tokenized\n", yytext);
				break;
			}
			c = yylex();
		}
	}else{
		c = yylex();
		while(newL == 0 || other == 1){
			if(c == NEWL){
				if(other == 1){
					other = 0;
				}else{
					newL = 1;
				} 
			}else{
				other = 1;
				newL = 0;
				if(c == VALUE){
					fprintf(fileOut, "VALUE\n");
					printf("VALUE\n");
				}else if(c == AND){
					fprintf(fileOut, "KW_AND\n");
					printf("KW_AND\n");
				}else if(c == OR){
					fprintf(fileOut, "KW_OR\n");
					printf("KW_OR\n");
				}else if(c == NOT){
					fprintf(fileOut, "KW_NOT\n");
					printf("KW_NOT\n");
				}else if(c == EQUAL){
					fprintf(fileOut, "KW_EQUAL\n");
					printf("KW_EQUAL\n");
				}else if(c == LESS){
					fprintf(fileOut, "KW_LESS\n");
					printf("KW_LESS\n");
				}else if(c == NIL){
					fprintf(fileOut, "KW_NIL\n");
					printf("KW_NIL\n");
				}else if(c == LIST){
					fprintf(fileOut, "KW_LIST\n");
					printf("KW_LIST\n");
				}else if(c == APPEND){
					fprintf(fileOut, "KW_APPEND\n");
					printf("KW_APPEND\n");
				}else if(c == CONCAT){
					fprintf(fileOut, "KW_CONCAT\n");
					printf("KW_CONCAT\n");
				}else if(c == SET){
					fprintf(fileOut, "KW_SET\n");
					printf("KW_SET\n");
				}else if(c == DEFFUN){
					fprintf(fileOut, "KW_DEFFUN\n");
					printf("KW_DEFFUN\n");				
				}else if(c == FOR){
					fprintf(fileOut, "KW_FOR\n");
					printf("KW_FOR\n");
				}else if(c == IF){
					fprintf(fileOut, "KW_IF\n");
					printf("KW_IF\n");
				}else if(c == EXIT){
					fprintf(fileOut, "KW_EXIT\n");
					printf("KW_EXIT\n");
				}else if(c == LOAD){
					fprintf(fileOut, "KW_LOAD\n");
					printf("KW_LOAD\n");
				}else if(c == DISP){
					fprintf(fileOut, "KW_DISP\n");
					printf("KW_DISP\n");
				}else if(c == TRUE){
					fprintf(fileOut, "KW_TRUE\n");
					printf("KW_TRUE\n");
				}else if(c == FALSE){
					fprintf(fileOut, "KW_FALSE\n");
					printf("KW_FALSE\n");
				}else if(c == PLUS){
					fprintf(fileOut, "OP_PLUS\n");
					printf("OP_PLUS\n");
				}else if(c == MINUS){
					fprintf(fileOut, "OP_MINUS\n");
					printf("OP_MINUS\n");
				}else if(c == DIV){
					fprintf(fileOut, "OP_DIV\n");
					printf("OP_DIV\n");
				}else if(c == MULT){
					fprintf(fileOut, "OP_MULT\n");
					printf("OP_MULT\n");
				}else if(c == OP){
					fprintf(fileOut, "OP_OP\n");
					printf("OP_OP\n");
				}else if(c == CP){
					fprintf(fileOut, "OP_CP\n");
					printf("OP_CP\n");
				}else if(c == DMULT){
					fprintf(fileOut, "OP_DBLMULT\n");
					printf("OP_DBLMULT\n");
				}else if(c == COMMA){
					fprintf(fileOut, "OP_COMMA\n");
					printf("OP_COMMA\n");
				}else if(c == QUOTE){
					quoteCounter++;
					if(quoteCounter % 2 == 0){
						fprintf(fileOut, "OP_CC\n");
						printf("OP_CC\n");
					}else{
						fprintf(fileOut, "OP_OC\n");
						printf("OP_OC\n");
					}
				}else if(c == COMMENT){
					fprintf(fileOut, "COMMENT\n");
					printf("COMMENT\n");
				}else if(c == IDENT){
					fprintf(fileOut, "IDENTIFIER\n");
					printf("IDENTIFIER\n");
				}else if(c == ERROR){
					fprintf(fileOut, "SYNTAX_ERROR  %s can not be tokenized\n", yytext);
					printf("SYNTAX_ERROR  %s can not be tokenized\n", yytext);
					break;
				}
			}
			if(newL != 1)
				c = yylex();
		}
	}
	fclose(fileOut);
	return 0;
}