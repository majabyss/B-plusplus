/*
 * token.c - Tokenizer (lexer) for B++ project
 * Reads source code and splits it into tokens: identifiers, keywords, numbers, strings, chars, operators, punctuation, etc.
 * Usage: ./token file.c
 */
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

// Token types
typedef enum {
	T_EOF, T_ID, T_NUM, T_STR, T_CHAR,
	T_OP, T_KW,
	T_PUNC,
	T_UNKNOWN
} tokentype;

// Keyword kinds
typedef enum {
	KW_IF, KW_ELSE, KW_ELIF, KW_WHILE, KW_FOR, KW_SWITCH, KW_CASE, KW_RETURN,
	KW_STRUCT, KW_DEFAULT, KW_STATIC, KW_CONT, KW_DO, KW_EX, KW_BREAK, KW_AUTO, 
	KW_GOTO, KW_SIZEOF, KW_EXTERN,
	KW_NONE // Not a keyword
} KwKind;

// Token structure
typedef struct {
	tokentype type;
	const char *start;
	size_t len;
	size_t line, col;
	KwKind kw; // If type == T_KW, which keyword
} token;

// Lexer state
typedef struct {
	const char *src;
	const char *p;
	size_t line, col;
} lexer;

// Returns 1 if c is a valid identifier start character (letter or _)
static int is_ident_start(int c) {
    return isalpha(c) || c == '_';
}

// Returns 1 if c is a valid identifier continuation character (letter, digit, or _)
static int is_ident_continue(int c) {
    return isalnum(c) || c == '_';
}

// Advances the lexer if the next character matches 'expect'
static int match(lexer *pl, char expect) {
    if (*pl->p == expect) {
        pl->p++; pl->col++;
        return 1;
    }
    return 0;
}

// Advances the lexer position, updating line and column numbers
static void posup(lexer *pl) {
    if (*pl->p == '\n') {
        pl->line++;
        pl->col = 1;
    } else {
        pl->col++;
    }
    pl->p++;
}

// Skips whitespace and comments (// and /* ... */)
static void skip_wscomms(lexer *pl) {
    for (;;) {
        while (isspace((unsigned char)*pl->p)) posup(pl);
        // Single-line comment
        if (pl->p[0] == '/' && pl->p[1] == '/') {
            while (*pl->p && *pl->p != '\n') posup(pl);
            continue;
        }
        // Multi-line comment
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

// Tries to match an operator at the current lexer position
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

// List of keyword strings and their corresponding KwKind values
static const struct { const char *name; KwKind kind; } KEYWORDS[] = {
    {"if", KW_IF}, {"else", KW_ELSE}, {"elif", KW_ELIF}, {"while", KW_WHILE},
    {"for", KW_FOR}, {"switch", KW_SWITCH}, {"case", KW_CASE}, {"return", KW_RETURN},
    {"struct", KW_STRUCT}, {"default", KW_DEFAULT}, {"static", KW_STATIC}, {"cont", KW_CONT},
    {"do", KW_DO}, {"ex", KW_EX}, {"break", KW_BREAK}, {"auto", KW_AUTO},
    {"goto", KW_GOTO}, {"sizeof", KW_SIZEOF}, {"extern", KW_EXTERN}
};
static const size_t NKEYWORDS = sizeof(KEYWORDS)/sizeof(KEYWORDS[0]);

// Checks if an identifier matches a keyword; returns KW_NONE if not
static KwKind lookup_keyword(const char *str, size_t len) {
    for (size_t i = 0; i < NKEYWORDS; i++) {
        if (strlen(KEYWORDS[i].name) == len && strncmp(str, KEYWORDS[i].name, len) == 0)
            return KEYWORDS[i].kind;
    }
    return KW_NONE;
}

/*
 * next_token - Lexes the next token from the input stream.
 * Handles identifiers, keywords, numbers, strings, chars, operators, punctuation, and unknown tokens.
 * Returns a token struct with type, position, and value info.
 */
static token next_token(lexer *pl) {
    skip_wscomms(pl);

    token t = {.type = T_UNKNOWN, .start = pl->p, .len = 0, .line = pl->line, .col = pl->col, .kw = KW_NONE};
    if(*pl->p == '\0') {
        t.type = T_EOF;
        return t;
    }

    // Identifier or keyword
    if (is_ident_start((unsigned char)*pl->p)) {
        const char *str = pl->p;
        posup(pl);
        while  (is_ident_continue((unsigned char )*pl->p)) posup(pl);
        size_t len = (size_t)(pl->p - str);
        KwKind kw = lookup_keyword(str, len);
        if (kw != KW_NONE) {
            t.type = T_KW; t.kw = kw;
        } else {
            t.type = T_ID; t.kw = KW_NONE;
        }
        t.start = str; t.len = len;
        return t;
    }

    // Number literal (decimal only)
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

    // String literal with error handling for unterminated string
    if (*pl->p == '"') {
        const char *str = pl->p;
        posup(pl);
        int closed = 0;
        while (*pl->p && *pl->p != '"') {
            if (*pl->p == '\\' && pl->p[1]) {posup(pl);}
            posup(pl);
        }
        if (*pl->p == '"') { posup(pl); closed = 1; }
        t.type = T_STR; t.start = str; t.len = (size_t)(pl->p - str);
        if (!closed) t.type = T_UNKNOWN; // Mark as unknown if unterminated
        return t;
    }

    // Character literal with error handling for unterminated char
    if (*pl->p == '\'') {
        const char *str = pl->p;
        posup(pl);
        int closed = 0;
        if (*pl->p == '\\' && pl->p[1]) {
            posup(pl);
            posup(pl);
        } else if (*pl->p) {posup(pl);}
        if (*pl->p == '\'') { posup(pl); closed = 1; }
        t.type = T_CHAR; t.start = str; t.len = (size_t)(pl->p - str);
        if (!closed) t.type = T_UNKNOWN;
        return t;
    }

    // Operator or punctuation
    size_t oplen = 0;
    if (try_match_op(pl->p, &oplen)) {
        t.start = pl->p; t.len = oplen;
        if (oplen == 1 && strchr(";(),{}[]", *pl->p)) t.type = T_PUNC;
        else t.type = T_OP;
        for (size_t i = 0; i < oplen; i++) posup(pl);
        return t;
    }

    // Unknown/invalid character
    posup(pl);
    t.type = T_UNKNOWN; t.len = 1;
    return t;
}

/*
 * slurp_file - Reads the entire contents of a file into a null-terminated buffer.
 * Returns a pointer to the buffer (must be freed by caller), or NULL on error.
 */
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

/*
 * print_token - Prints a token's type, position, and value to stdout.
 */
static void print_token(const token *t) {
    printf("[%zu:%zu] ", t->line, t->col);
    switch (t->type) {
        case T_ID: printf("ID    : "); break;
        case T_KW: printf("KW    : "); break;
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

/*
 * Main entry point: reads a file, tokenizes it, and prints all tokens.
 */
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
