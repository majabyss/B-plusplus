#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

// A simple lexer for a subset of C language
typedef enum {
	T_EOF, T_ID, T_NUM, T_STR, T_CHAR,
	T_OP, T_KW,
	T_PUNC,
	T_UNKNOWN
} tokentype;

// Keywords
typedef enum {
	KW_IF, KW_ELSE, KW_ELIF, KW_WHILE, KW_FOR, KW_SWITCH, KW_CASE, KW_RETURN,
	KW_STRUCT, KW_DEFAULT, KW_STATIC, KW_CONT, KW_DO, KW_EX, KW_BREAK, KW_AUTO, 
	KW_GOTO, KW_SIZEOF, KW_EXTERN
} KwKind;

// List of keywords
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

// Function prototypes
static int is_ident_start(int c) {
	return isalpha(c) || c == '_';
}
// Check if character can be part of an identifier (after the first character)
static int is_ident_continue(int c) {
	return isalnum(c) || c == '_';
}

// Match a specific character and advance the lexer position if matched
static int match(lexer *pl, char expect) {
	if (*pl->p == expect) {
		pl->p++; pl->col++; 
		return 1;
	}
	return 0;
}

// Advance the lexer position, updating line and column numbers
static void posup(lexer *pl) {
	if (*pl->p == '\n') {
		pl->line++; 
		pl->col = 1;
	} else {
		pl->col++;
	}
	pl->p++;
}

// Skip whitespace and comments
static void skip_wscomms(lexer *pl) {
	for (;;) {
		while (isspace((unsigned char)*pl->p)) posup(pl);

        // Single-line comment
		if (pl->p[0] == '/' && pl->p[1] == '/') {
            // Skip to end of line
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

// List of operators and punctuators
static const char *OPS[] = {
	">>=", "<<=", "==", "!=", "<=", ">=", "&&", "||", "++", "--",
	"+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "->", "<<", ">>",
	"##","+","-","*","/","%","&","|","^","~","!","<",">","=","?",":",".",
	";",",","#","(",")","{","}","[","]"
};

// Number of operators
static const size_t NOPS = sizeof(OPS)/sizeof(OPS[0]);

// Try to match an operator at the current position
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

// Get the next token from the input
static token next_token(lexer *pl) {
	skip_wscomms(pl);
	
    // Initialize token
	token t = {.type = T_UNKNOWN, .start = pl->p, .len = 0, .line = pl->line, .col = pl->col};
	if(*pl->p == '\0') {
		t.type = T_EOF;
		return t;
	}
// Identifier or keyword
	if (is_ident_start((unsigned char)*pl->p)) {
		const char *str = pl->p;
		posup(pl);
        // Continue while valid identifier characters
		while  (is_ident_continue((unsigned char )*pl->p)) posup(pl);
		t.type = T_ID; t.start = str; t.len = (size_t)(pl->p - str);
		return t;
	}
// Number (integer or floating-point)
	if (isdigit((unsigned char)*pl->p)) {
		const char *str = pl->p;
		while (isdigit((unsigned char)*pl->p)) posup(pl);

        // Handle decimal point and fractional part
		if (*pl->p == '.' && isdigit((unsigned char)pl->p[1])) {
			posup(pl);
			while (isdigit((unsigned char)*pl->p)) posup(pl);
		}
		t.type = T_NUM; t.start = str; t.len = (size_t)(pl->p - str);
		return t;
	}

    // String literal
	if (*pl->p == '"') {
		const char *str = pl->p;
		posup(pl);
		while (*pl->p && *pl->p != '"') {
			if (*pl->p == '\\' && pl->p[1]) {posup(pl);}
			posup(pl);
		}
        // Closing quote
		if (*pl->p == '"') posup(pl);
		t.type = T_STR; t.start = str; t.len = (size_t)(pl->p - str);
		return t;
	}

    // Character literal
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

// Read entire file into a buffer
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

// Print token information
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
// Main function to run the lexer
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

// Initialize symbol table
static symbol_table *init_symbol_table(void) {
	symbol_table *st = malloc(sizeof(symbol_table));
	if (!st) return NULL;
	st->capacity = 64;
	st->count = 0;
	st->symbols = malloc(st->capacity * sizeof(char*));
	if (!st->symbols) {
		free(st);
		return NULL;
	}
	return st;
}

// Add a symbol to the table
static void add_symbol(symbol_table *st, const char *name, size_t len) {
	if (st->count >= st->capacity) {
		st->capacity *= 2;
		char **new_symbols = realloc(st->symbols, st->capacity * sizeof(char*));
		if (!new_symbols) return;
		st->symbols = new_symbols;
	}
	st->symbols[st->count] = malloc(len + 1);
	if (!st->symbols[st->count]) return;
	strncpy(st->symbols[st->count], name, len);
	st->symbols[st->count][len] = '\0';
	st->count++;
}

// Check if a symbol exists in the table
static int symbol_exists(symbol_table *st, const char *name, size_t len) {
	for (size_t i = 0; i < st->count; i++) {
		if (strlen(st->symbols[i]) == len && strncmp(st->symbols[i], name, len) == 0) {
			return 1;
		}
	}
	return 0;
}

// Free symbol table
static void free_symbol_table(symbol_table *st) {
	if (!st) return;
	for (size_t i = 0; i < st->count; i++) {
		free(st->symbols[i]);
	}
	free(st->symbols);
	free(st);
}

// Helper function to check if identifier matches a string
static int token_id_matches(const token *t, const char *str) {
	if (t->type != T_ID) return 0;
	size_t slen = strlen(str);
	return t->len == slen && strncmp(t->start, str, slen) == 0;
}

// Helper function to get current token
static token *current_token(parser *p) {
	if (p->pos >= p->count) return &p->tokens[p->count - 1]; // Return EOF
	return &p->tokens[p->pos];
}

// Helper function to advance to next token
static void advance(parser *p) {
	if (p->pos < p->count - 1) p->pos++;
}

// Helper function to write token value to output
static void write_token(parser *p, const token *t) {
	fwrite(t->start, 1, t->len, p->out);
}

// Helper function to expect and consume a specific punctuation
static int expect_punc(parser *p, const char *punc) {
	token *t = current_token(p);
	if (t->type == T_PUNC && t->len == strlen(punc) && strncmp(t->start, punc, t->len) == 0) {
		advance(p);
		return 1;
	}
	return 0;
}

// Add error reporting function
static void report_error(parser *p, const token *t, const char *msg) {
	fprintf(stderr, "Error at [%zu:%zu]: %s '", t->line, t->col, msg);
	fwrite(t->start, 1, t->len, stderr);
	fprintf(stderr, "'\n");
	p->has_error = 1;
}

//Parse types to C
static const char *translate_type(const token *t) {
	if (token_id_matches(t, "int")) return "int";
	if (token_id_matches(t, "i8")) return "int8_t";
	if (token_id_matches(t, "i16")) return "int16_t";
	if (token_id_matches(t, "i32")) return "int32_t";
	if (token_id_matches(t, "i64")) return "int64_t";
	if (token_id_matches(t, "u8")) return "uint8_t";
	if (token_id_matches(t, "u16")) return "uint16_t";
	if (token_id_matches(t, "u32")) return "uint32_t";
	if (token_id_matches(t, "u64")) return "uint64_t";
	if (token_id_matches(t, "float")) return "float";
	if (token_id_matches(t, "f32")) return "float";
	if (token_id_matches(t, "f64")) return "double";
	if (token_id_matches(t, "void")) return "void";
	if (token_id_matches(t, "chr")) return "char";
 	if (token_id_matches(t, "memsize")) return "size_t";
  	if (token_id_matches(t, "NULL")) return "NULL"; // Not a type
}

// Memory safety Check
static void address_checker() {

}

// Validate identifier - check if it's defined
static int validate_identifier(parser *p, const token *t) {
	// Check if it's a built-in function you want to allow
	if (token_id_matches(t, "printf")) return 1;  // Allow printf
  if (token_id_matches(t, "malloc")) return 1;  // Allow malloc
  if (token_id_matches(t, "free")) return 1;  //Allow free

	// Check if it's in the symbol table
	if (symbol_exists(p->symbols, t->start, t->len)) {
		return 1;
	}
	
	// Check if it's a type (types don't need to be in symbol table)
	if (translate_type(t) != NULL) {
		return 1;
	}
	
	// Unknown identifier
	report_error(p, t, "Undefined identifier");
	return 0;
}

// Forward declaration for parsing statements
static void parse_statement(parser *p);
static void parse_block(parser *p);

// Parse function declaration
static void parse_function(parser *p) {
	fprintf(p->out, "// Function declaration\n");
	advance(p); // consume "fun"
	
	// Get return type
	token *ret_type = current_token(p);
	const char *c_type = translate_type(ret_type);
	if (c_type) {
		fprintf(p->out, "%s ", c_type);
		advance(p);
	} else {
		fprintf(p->out, "void ");
	}
	
	// Get function name and register it
	token *fn_name = current_token(p);
	if (fn_name->type == T_ID) {
		add_symbol(p->symbols, fn_name->start, fn_name->len);
		write_token(p, fn_name);
		advance(p);
	}
	
	// Parse parameters
	if (expect_punc(p, "(")) {
		fprintf(p->out, "(");
		while (current_token(p)->type != T_EOF && !expect_punc(p, ")")) {
			token *param_type = current_token(p);
			const char *c_param_type = translate_type(param_type);
			if (c_param_type) {
				fprintf(p->out, "%s ", c_param_type);
				advance(p);
			}
			
			token *param_name = current_token(p);
			if (param_name->type == T_ID) {
				add_symbol(p->symbols, param_name->start, param_name->len);
				write_token(p, param_name);
				advance(p);
			}
			
			if (!expect_punc(p, ",")) break;
			fprintf(p->out, ", ");
		}
		fprintf(p->out, ")");
	}
	
	// Parse function body
	if (expect_punc(p, "{")) {
		fprintf(p->out, " {\n");
		while (current_token(p)->type != T_EOF && !expect_punc(p, "}")) {
			parse_statement(p);
		}
		fprintf(p->out, "}\n\n");
	}
}

// Parse variable declaration
static void parse_var_declaration(parser *p) {
    token *var_type = current_token(p);
    const char *c_type = translate_type(var_type);
    if (!c_type) return;

    // Emit base C type
    fprintf(p->out, "    %s", c_type);
    advance(p); // consume the type token

    // Collect pointers WITH ORIGINAL SPACING
    while (current_token(p)->type == T_OP && current_token(p)->len == 1 && *current_token(p)->start == '*') {
        token *ptr_tok = current_token(p);

        for (int i = 0; i < ptr_tok->leading_spaces && i < 2; i++) {
            fprintf(p->out, " ");
    }
    fprintf(p->out, "*");
        advance(p);
  }

     // Write variable name with its original spacing
    token *var_name = current_token(p);
    if (var_name->type == T_ID) {
        // Preserve spacing before variable name
        for (int i = 0; i < var_name->leading_spaces && i < 2; i++) {
            fprintf(p->out, " ");
        }
        add_symbol(p->symbols, var_name->start, var_name->len);
        write_token(p, var_name);
        advance(p);
    }

    // Handle initialization (consume '=' and the expression)
    if (current_token(p)->type == T_OP && current_token(p)->len == 1 && *current_token(p)->start == '=') {
        fprintf(p->out, " = ");
        advance(p); // consume '='

        // Parse expression - need to handle parentheses!
        int paren_depth = 0;
        while ((current_token(p)->type != T_PUNC || paren_depth > 0) && 
               current_token(p)->type != T_EOF) {
            token *val = current_token(p);
            
            // Track parentheses depth
            if (val->type == T_PUNC && val->len == 1) {
                if (*val->start == '(') paren_depth++;
                else if (*val->start == ')') paren_depth--;
                else if (*val->start == ';' && paren_depth == 0) break;  // End of statement
            }
            
            if (val->type == T_ID) {
                if (!validate_identifier(p, val)) {
                    advance(p);
                    continue;
                }
            }
            write_token(p, val);
            advance(p);
        }
    }

    // Consume trailing semicolon if present
    if (expect_punc(p, ";")) {
        fprintf(p->out, ";\n");
    }
}

// Parse a generic statement
static void parse_statement(parser *p) {
	token *t = current_token(p);
	
	// Check if it's a type declaration
	const char *c_type = translate_type(t);
	if (c_type) {
		parse_var_declaration(p);
		return;
	}
	
	// Handle keywords
	if (t->type == T_KW) {
		switch (t->kw) {
			case KW_RETURN:
				fprintf(p->out, "return");
				advance(p);
				if (current_token(p)->type != T_PUNC) {
					fprintf(p->out, " ");
					while (current_token(p)->type != T_PUNC && current_token(p)->type != T_EOF) {
						token *ret_val = current_token(p);
						if (ret_val->type == T_ID && !validate_identifier(p, ret_val)) {
							advance(p);
							continue;
						}
						write_token(p, ret_val);
						fprintf(p->out, " ");
						advance(p);
					}
				}
				if (expect_punc(p, ";")) {
					fprintf(p->out, ";\n");
				}
				return;
			case KW_IF:
				fprintf(p->out, "if");
				advance(p);
				if (expect_punc(p, "(")) {
					fprintf(p->out, "(");
					while (!expect_punc(p, ")") && current_token(p)->type != T_EOF) {
						token *expr = current_token(p);
						if (expr->type == T_ID && !validate_identifier(p, expr)) {
							advance(p);
							continue;
						}
						write_token(p, expr);
						fprintf(p->out, " ");
						advance(p);
					}
					fprintf(p->out, ")");
				}
				if (expect_punc(p, "{")) {
					fprintf(p->out, " {\n");
					while (!expect_punc(p, "}") && current_token(p)->type != T_EOF) {
						parse_statement(p);
					}
					fprintf(p->out, "}\n");
				}
				return;
	    		case KW_ELIF:
				fprintf(p->out, "else if");
				advance(p);
				if (expect_punc(p, "(")) {
					fprintf(p->out, "(");
					while (!expect_punc(p, ")") && current_token(p)->type != T_EOF) {
						token *expr = current_token(p);
						if (expr->type == T_ID && !validate_identifier(p, expr)) {
							advance(p);
							continue;
						}
						write_token(p, expr);
						fprintf(p->out, " ");
						advance(p);
					}
					fprintf(p->out, ")");
				}
				if (expect_punc(p, "{")) {
					fprintf(p->out, " {\n");
					while (!expect_punc(p, "}") && current_token(p)->type != T_EOF) {
						parse_statement(p);
					}
					fprintf(p->out, "}\n");
				}
				return;
	    		case KW_ELSE:
				fprintf(p->out, "else");
				advance(p);
				if (expect_punc(p, "{")) {
					fprintf(p->out, " {\n");
					while (!expect_punc(p, "}") && current_token(p)->type != T_EOF) {
						parse_statement(p);
					}
					fprintf(p->out, "}\n");
				}
				return;
			case KW_WHILE:
				fprintf(p->out, "while");
				advance(p);
				if (expect_punc(p, "(")) {
					fprintf(p->out, "(");
					while (!expect_punc(p, ")") && current_token(p)->type != T_EOF) {
						token *arg = current_token(p);
						if (arg->type == T_ID && !validate_identifier(p, arg)) {
							advance(p);
							continue;
						}
						write_token(p, arg);
						advance(p);
					}
					fprintf(p->out, ")");
				}
				if (expect_punc(p, "{")) {
					fprintf(p->out, "{");
					while (!expect_punc(p, "}") && current_token(p)->type != T_EOF) {
						parse_statement(p);
					}
					fprintf(p->out, "}\n");
				}
				return;
			default:
			break;
		}
	}
	
	// Generic statement - validate identifiers
	fprintf(p->out, "    ");
	while (current_token(p)->type != T_EOF && !expect_punc(p, ";")) {
		token *tok = current_token(p);
		if (tok->type == T_ID && !validate_identifier(p, tok)) {
			advance(p);
			continue;
		}
		write_token(p, tok);
		advance(p);
	}
	fprintf(p->out, ";\n");
}

// Main parser function
static void parse(parser *p) {
	fprintf(p->out, "#include <stdio.h>\n");
	fprintf(p->out, "#include <stdint.h>\n");
	fprintf(p->out, "#include <string.h>\n");
	fprintf(p->out, "#include <stdlib.h>\n");
	fprintf(p->out, "#include <stdbool.h>\n\n");
	
	while (p->pos < p->count) {
		token *t = current_token(p);
		
		if (t->type == T_EOF) break;
		
		// Check for function declaration
		if (token_id_matches(t, "fun")) {
			parse_function(p);
		} else {
			// Skip unknown tokens at top level
			advance(p);
		}
	}
}

// Tokenize entire file into array
static token *tokenize_all(lexer *pl, size_t *out_count) {
	size_t cap = 256;
	size_t cnt = 0;
	token *tokens = malloc(cap * sizeof(token));
	if (!tokens) return NULL;
	
	for (;;) {
		token t = next_token(pl);
		if (cnt >= cap) {
			cap *= 2;
			token *new_tokens = realloc(tokens, cap * sizeof(token));
			if (!new_tokens) {
				free(tokens);
				return NULL;
			}
			tokens = new_tokens;
		}
		tokens[cnt++] = t;
		if (t.type == T_EOF) break;
	}
	
	*out_count = cnt;
	return tokens;
}

/*
* Main entry point: reads a file, tokenizes it, and prints all tokens.
*/
int main(int argc, char **argv) {
	if (argc < 2) {
		fprintf(stderr, "usage: %s file.bpp [output.c]\n", argv[0]);
		return 1;
	}
	char *buf = slurp_file(argv[1]);
	if (!buf) {
		perror("file");
		return 1;
	}
	
	// Determine output file
	const char *out_filename = (argc >= 3) ? argv[2] : "output.c";
	
	lexer pl = {.src = buf, .p = buf, .line = 1, .col = 1};
	
	// Tokenize entire file
	size_t token_count;
	token *tokens = tokenize_all(&pl, &token_count);
	if (!tokens) {
		fprintf(stderr, "Error: failed to tokenize\n");
		free(buf);
		return 1;
	}
	
	// Open output file
	FILE *out = fopen(out_filename, "w");
	if (!out) {
		perror("output file");
		free(tokens);
		free(buf);
		return 1;
	}
	
	// Initialize symbol table
	symbol_table *symbols = init_symbol_table();
	if (!symbols) {
		fprintf(stderr, "Error: failed to initialize symbol table\n");
		fclose(out);
		free(tokens);
		free(buf);
		return 1;
	}
	
	// Initialize parser with symbol table
	parser p = {
		.tokens = tokens, 
		.count = token_count, 
		.pos = 0, 
		.out = out,
		.symbols = symbols,
		.has_error = 0
	};
	parse(&p);
	
	fclose(out);
	
	// Check for errors before compiling
	if (p.has_error) {
		fprintf(stderr, "\nCompilation failed due to errors.\n");
		free_symbol_table(symbols);
		free(tokens);
		free(buf);
		return 1;
	}
	
	printf("Output written to %s\n", out_filename);
	
	free_symbol_table(symbols);
	free(tokens);
	free(buf);
	
	char command[256];
	snprintf(command, sizeof(command), "gcc -o output %s", out_filename);
	int result = system(command);
	return 0;
}
