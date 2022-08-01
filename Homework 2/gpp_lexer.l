%option noyywrap
%{
	#include<stdio.h>
	#include<string.h>
	#include "gpp_interpreter.h"

	int quoteOpen = 0;
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
{value}		      {yylval.value = atoi(yytext); return VALUE;};
{real}		      {yylval.value = atof(yytext); return VALUE;};
"and"|"AND"	      return KW_AND;
"or"|"OR"	      return KW_OR;
"not"|"NOT"	      return KW_NOT;
"equal"|"EQUAL"	      return KW_EQUAL;
"less"|"LESS"	      return KW_LESS;
"nil"|"NIL"           return KW_NIL;
"list"|"LIST"	      return KW_LIST;
"append"|"APPEND"     return KW_APPEND;
"concat"|"CONCAT"     return KW_CONCAT;
"set"|"SET"           return KW_SET;
"deffun"|"DEFFUN"     return KW_DEFFUN;
"for"|"FOR"           return KW_FOR;
"if"|"IF"             return KW_IF;
"exit"|"EXIT"         return KW_EXIT;
"load"|"LOAD"         return KW_LOAD;
"disp"|"DISP"         return KW_DISP;
"true"|"TRUE"         return KW_TRUE;
"false"|"FALSE"       return KW_FALSE;
"+"		      return OP_PLUS;	
"-"		      return OP_MINUS;
"/"		      return OP_DIV;
"*"		      return OP_MULT;
"("		      return OP_OP;
")"		      return OP_CP;
"**"		      return OP_DBLMULT;
","		      return OP_COMMA;
\"                    {if(quoteOpen = !quoteOpen)return OP_OC; return OP_CC;}
{errorValue}          return 0;
{errorReal}	      return 0;
{errorIdent}	      return 0;
{comment}	      return COMMENT;
" "		      { ; }
{identifier}	      {strcpy(yylval.id, yytext); return IDENTIFIER;}
<<EOF>>		      return 0;
\r		      { ; }
\t		      { ; }
\tab		      { ; }
\n                    { ; }
.		      return 0;
%%