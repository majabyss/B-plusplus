#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

typedef enum {
	T_EOF, T_ID, T_NUM, T_STR, T_CHAR,
	T_OP,
	T_PUNC,
	T_UNKNOWN
} tokentype;

typedef enum {
	KW_IF, KW_ELSE, KW_ELIF, KW_WHILE, KW_FOR, KW_SWITCH, KW_CASE, KW_RETURN,
	KW_STRUCT, KW_DEFAULT, KW_STATIC, KW_CONT, KW_DO, KW_EX, KW_BREAK, KW_AUTO, 
	KW_GOTO, KW_SIZEOF, KW_EXTERN
} KwKind;

typedef struct {
	tokentype type;
	const char *start;
	size_t len;
	size_t line, col;
} token;

typedef struct {
	const char *src;
	const char *p;
	size_t line, col;
} lexer;

static int is_ident_start(int c) {
	return isalpha(c) || c == '_';
}

static int is_ident_continue(int c) {
	return isalnum(c) || c == '_';
}

static int match(lexer *pl, char expect) {
	if (*pl->p == expect) {
		pl->p++; pl->col++; 
		return 1;
	}
	return 0;
}

static void posup(lexer *pl) {
	if (*pl->p == '\n') {
		pl->line++; 
		pl->col = 1;
	} else {
		pl->col++;
	}
	pl->p++;
}

static void skip_wscomms(lexer *pl) {
	for (;;) {
		while (isspace((unsigned char)*pl->p)) posup(pl);

		if (pl->p[0] == '/' && pl->p[1] == '/') {
			while (*pl->p && *pl->p != '\n') posup(pl);
			continue;
		}

		if (pl->p[0] == '/' && pl->p[1] == '*') {
			pl->p += 2; pl->col += 2;
			while (*pl->p) {
				if (pl->p[0] == '*' && pl->p[1] == '/') {
					pl->p += 2; pl->col += 2;
					break;
				}
				posup(pl);
			}
			continue;
		}
		break;
	}
}

static const char *OPS[] = {
	">>=", "<<=", "==", "!=", "<=", ">=", "&&", "||", "++", "--",
	"+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "->", "<<", ">>",
	"##","+","-","*","/","%","&","|","^","~","!","<",">","=","?",":",".",
	";",",","#","(",")","{","}","[","]"
};

static const size_t NOPS = sizeof(OPS)/sizeof(OPS[0]);

static int try_match_op(const char *p, size_t *out_len) {
	for (size_t i = 0; i < NOPS; i++) {
		size_t klen = strlen(OPS[i]);
		if (strncmp(p, OPS[i], klen) == 0) {
			*out_len = klen;
			return 1;
		}
	}
	return 0;
}

static token next_token(lexer *pl) {
	skip_wscomms(pl);
	
	token t = {.type = T_UNKNOWN, .start = pl->p, .len = 0, .line = pl->line, .col = pl->col};
	if(*pl->p == '\0') {
		t.type = T_EOF;
		return t;
	}

	if (is_ident_start((unsigned char)*pl->p)) {
		const char *str = pl->p;
		posup(pl);
		while  (is_ident_continue((unsigned char )*pl->p)) posup(pl);
		t.type = T_ID; t.start = str; t.len = (size_t)(pl->p - str);
		return t;
	}

	if (isdigit((unsigned char)*pl->p)) {
		const char *str = pl->p;
		while (isdigit((unsigned char)*pl->p)) posup(pl);

		if (*pl->p == '.' && isdigit((unsigned char)pl->p[1])) {
			posup(pl);
			while (isdigit((unsigned char)*pl->p)) posup(pl);
		}
		t.type = T_NUM; t.start = str; t.len = (size_t)(pl->p - str);
		return t;
	}

	if (*pl->p == '"') {
		const char *str = pl->p;
		posup(pl);
		while (*pl->p && *pl->p != '"') {
			if (*pl->p == '\\' && pl->p[1]) {posup(pl);}
			posup(pl);
		}
		if (*pl->p == '"') posup(pl);
		t.type = T_STR; t.start = str; t.len = (size_t)(pl->p - str);
		return t;
	}

	if (*pl->p == '\'') {
		const char *str = pl->p;
		posup(pl);
		if (*pl->p == '\\' && pl->p[1]) {
			posup(pl);
			posup(pl);
		} else if (*pl->p) {posup(pl);}
		if (*pl->p == '\'') posup(pl);
		t.type = T_CHAR; t.start = str; t.len = (size_t)(pl->p - str);
		return t;
	}

	size_t oplen = 0;
	if (try_match_op(pl->p, &oplen)) {
		t.start = pl->p; t.len = oplen;
		if (oplen == 1 && strchr(";(),{}[]", *pl->p)) t.type = T_PUNC;
		else t.type = T_OP;
		for (size_t i = 0; i < oplen; i++) posup(pl);
		return t;
	}

	posup(pl);
	t.type = T_UNKNOWN; t.len = 1;
	return t;
}

static char *slurp_file(const char *fn) {
	FILE *pf = fopen(fn, "rb");
	if (!pf) return NULL;
	fseek(pf, 0, SEEK_END);
	long n = ftell(pf);
	fseek(pf, 0, SEEK_SET);
	char *buf = malloc((size_t)n + 1);
	if (!buf) {
		fclose(pf);
		return NULL;
	}
	fread(buf, 1, (size_t)n, pf);
	buf[n] = '\0';
	fclose(pf);
	return buf;
}

static void print_token(const token *t) {
	printf("[%zu:%zu] ", t->line, t->col);
	switch (t->type) {
		case T_ID: printf("ID    : "); break;
		case T_NUM: printf("NUM   : "); break;
		case T_STR: printf("STR   : "); break;
		case T_CHAR: printf("CHAR  : "); break;
		case T_OP: printf("OP    : "); break;
		case T_PUNC: printf("PUNC  : "); break;
		case T_EOF: printf("EOF\n"); break;
		default: printf("UNKNOWN  : "); break;
	}
	fwrite(t->start, 1, t->len, stdout);
	putchar('\n');
}

int main(int argc, char **argv) {
	if (argc < 2) {
		fprintf(stderr, "usage: %s file.c\n", argv[0]);
		return 1;
	}
	char *buf = slurp_file(argv[1]);
	if (!buf) {
		perror("file");
		return 1;
	}
	lexer pl = {.src = buf, .p = buf, .line = 1, .col = 1};
	for (;;) {
		token t = next_token(&pl);
		print_token(&t);
		if (t.type == T_EOF) break;
	}
	free(buf);
	return 0;
}

/*
long filesize(const char* fn) {
	FILE *psi = fopen(fn, "rb");
	if (psi == NULL) {
		perror("Error opening file\n");
		return -1;
	}
	fseek(psi, 0, SEEK_END);
	long size = ftell(psi);
	fclose(psi);
	return size;
}

int main() {
	char fn[100];
	scanf("%s", fn);

	long length = filesize(fn);
	if (length < 0) return 1;
	
	FILE *pre = fopen(fn, "r");
	char line[length + 1];
	while (fgets(line, sizeof(line), pre)) {
		char *token = strtok(line, " \t\n");
		while  (token) {
			printf("%s\n", token);
			token = strtok(NULL, " \t\n");
		}
	}
	fclose(pre);
	return 0;
}
*/
