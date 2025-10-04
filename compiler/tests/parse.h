#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

int parse(const token *t) {
	for (;;) {
		if (t->type = T_ID) {
			switch (T_ID) {
				case "fun": //function declaration; break;
				case "i8": //8bit signed integer; break;
				case "i16": //16bit signed integer; break;
				case "i32": //32bit signed integer; break;
				case "i64": //64bit signed integer; break;
				case "u8": //8bit unisgned integer; break;
				case "u16": //16bit unsigned integer; break;
				case "u32": //32bit unisgned integer; break;
				case "u64": //64bit unsigned integer; break;
				case "f32": //32bit float; break;
				case "f64": //64bit float; break;
			}
		}
		if (t->type = T_PUNC) {
			switch (T_PUNC) {
				case ";": //end statement
				case "//": //comment
			}
		}
	}
}
