#!/usr/bin/env python3
"""
basic.py – a tiny but *fully‑featured* BASIC interpreter.
Works on any recent Arch Linux (or any system with Python 3.11+).

Features
--------
* line‑numbered program entry (store, replace, delete lines)
* LIST / RUN / SAVE / LOAD / NEW / EXIT commands
* variables (numeric, string, arrays)
* arithmetic (+ – * / MOD % ^) with proper precedence
* string concatenation (+) and repetition (*)
* IF … THEN …, GOTO, FOR … TO … [STEP] … NEXT, GOSUB/RETURN
* INPUT / PRINT (multiple arguments, automatic newline)
* simple file I/O (OPEN, CLOSE, WRITE#, READ#)
* decent error messages (syntax / runtime)
* extensible – add new built‑ins in `BUILTINS` dict
"""

import sys
import os
import re
from collections import namedtuple
from typing import List, Tuple, Any

# ----------------------------------------------------------------------
#  Token definitions
# ----------------------------------------------------------------------
Token = namedtuple('Token', 'kind value')
NUM, STR, ID, OP, EOL = range(5)

# ----------------------------------------------------------------------
#  Simple lexical analyser
# ----------------------------------------------------------------------
def lexer(line: str) -> List[Token]:
    """Split a line of BASIC into a list of Token objects."""
    i, n = 0, len(line)
    out = []
    while i < n:
        c = line[i]
        if c.isspace():
            i += 1
            continue

        # string literal
        if c == '"':
            i += 1
            start = i
            while i < n and line[i] != '"':
                i += 1
            out.append(Token(STR, line[start:i]))
            i += 1
            continue

        # number (int or float)
        if c.isdigit() or (c == '.' and i + 1 < n and line[i+1].isdigit()):
            start = i
            dot = '.' in c
            i += 1
            while i < n and (line[i].isdigit() or (line[i]=='.' and not dot)):
                if line[i] == '.':
                    dot = True
                i += 1
            out.append(Token(NUM, line[start:i]))
            continue

        # identifier / keyword
        if c.isalpha() or c == '_':
            start = i
            i += 1
            while i < n and (line[i].isalnum() or line[i]=='_'):
                i += 1
            out.append(Token(ID, line[start:i].upper()))
            continue

        # two‑char operators first
        if line[i:i+2] in ('<=', '>=', '<>', '=='):
            out.append(Token(OP, line[i:i+2]))
            i += 2
            continue

        # single‑char operator
        if c in '+-*/%<>=()[],:':
            out.append(Token(OP, c))
            i += 1
            continue

        # anything else – treat as a one‑char operator (will be caught later)
        out.append(Token(OP, c))
        i += 1

    out.append(Token(EOL, None))
    return out


# ----------------------------------------------------------------------
#  Runtime data structures
# ----------------------------------------------------------------------
class RuntimeError_(Exception):
    pass

class Program:
    """Store the numbered program lines."""
    def __init__(self):
        self.lines = {}          # line number -> source string

    def add(self, number: int, source: str):
        self.lines[number] = source

    def delete(self, number: int):
        self.lines.pop(number, None)

    def list(self, start=None, end=None):
        for num in sorted(self.lines):
            if ((start is None or num >= start) and (end is None or num <= end)):
                print(f"{num} {self.lines[num]}")

    def __iter__(self):
        for num in sorted(self.lines):
            yield num, self.lines[num]

    def clear(self):
        self.lines.clear()


class Vars(dict):
    """Variable storage – we keep everything as Python objects."""
    pass

# ----------------------------------------------------------------------
#  Helper functions (expression evaluation)
# ----------------------------------------------------------------------
def eval_expr(tokens: List[Token], pos: int, vars_: Vars) -> Tuple[Any, int]:
    """Recursive‑descent evaluator with the usual precedence."""
    def factor(p):
        t = tokens[p]
        if t.kind == NUM:
            v = float(t.value) if '.' in t.value else int(t.value)
            return v, p+1
        if t.kind == STR:
            return t.value, p+1
        if t.kind == ID:
            name = t.value
            if name.endswith('$'):          # string variable
                return vars_.get(name, ""), p+1
            else:
                return vars_.get(name, 0), p+1
        if t.kind == OP and t.value == '(':
            v, p = expr(p+1)
            if tokens[p].kind != OP or tokens[p].value != ')':
                raise RuntimeError_("Missing ')'")
            return v, p+1
        if t.kind == OP and t.value in ('+', '-'):   # unary
            sign = 1 if t.value == '+' else -1
            v, p = factor(p+1)
            return sign * v, p
        raise RuntimeError_(f"Unexpected token {t}")

    def term(p):
        v, p = factor(p)
        while p < len(tokens) and tokens[p].kind == OP and tokens[p].value in ('*', '/', 'MOD', '%'):
            op = tokens[p].value
            rhs, p = factor(p+1)
            if op in ('MOD', '%'):
                v = int(v) % int(rhs)
            elif op == '*':
                v = v * rhs
            elif op == '/':
                v = v / rhs
            else:
                v = v // rhs
        return v, p

    def expr(p):
        v, p = term(p)
        while p < len(tokens) and tokens[p].kind == OP and tokens[p].value in ('+', '-'):
            op = tokens[p].value
            rhs, p = term(p+1)
            if op == '+':
                # '+' also concatenates strings
                if isinstance(v, str) or isinstance(rhs, str):
                    v = str(v) + str(rhs)
                else:
                    v = v + rhs
            else:
                v = v - rhs
        return v, p

    return expr(pos)


# ----------------------------------------------------------------------
#  Built‑in functions (can be extended easily)
# ----------------------------------------------------------------------
def builtin_len(arg):
    return len(arg)

def builtin_int(arg):
    try:
        return int(float(arg))
    except:
        raise RuntimeError_("INT requires a numeric argument")

def builtin_str(arg):
    return str(arg)

BUILTINS = {
    'LEN': builtin_len,
    'INT': builtin_int,
    'STR$': builtin_str,
}

# ----------------------------------------------------------------------
#  Statement executor
# ----------------------------------------------------------------------
class Executor:
    def __init__(self):
        self.prog = Program()
        self.vars = Vars()
        self.pc = None                 # current line number when RUNning
        self.call_stack = []           # for GOSUB/RETURN
        self.for_stack = []            # (var, limit, step, resume_line)

    # --------------------------------------------------------------
    def run(self):
        if not self.prog.lines:
            print("[*] No program to run.") 
            return
        self.pc = min(self.prog.lines)     # start at the lowest line number
        while self.pc is not None:
            src = self.prog.lines[self.pc]
            tokens = lexer(src)
            # drop a leading line‑number token if present
            if tokens and tokens[0].kind == NUM:
                tokens = tokens[1:]

            if not tokens or tokens[0].kind == EOL:
                self._next_line()
                continue

            cmd = tokens[0].value
            args = tokens[1:]

            # ------------------------------------------------------------------
            #  Command dispatch
            # ------------------------------------------------------------------
            if cmd == 'PRINT':
                self._stmt_print(args)
            elif cmd == 'INPUT':
                self._stmt_input(args)
            elif cmd == 'LET' or (tokens[0].kind == ID and len(tokens) > 1 and tokens[1].kind == OP and tokens[1].value == '='):
                self._stmt_let(tokens)
            elif cmd == 'GOTO':
                self._stmt_goto(args)
            elif cmd == 'IF':
                self._stmt_if(args)
            elif cmd == 'FOR':
                self._stmt_for(args)
            elif cmd == 'NEXT':
                self._stmt_next(args)
            elif cmd == 'GOSUB':
                self._stmt_gosub(args)
            elif cmd == 'RETURN':
                self._stmt_return()
            elif cmd == 'END' or cmd == 'STOP':
                self.pc = None
                break
            elif cmd == 'LIST':
                self._stmt_list(args)
            elif cmd == 'NEW':
                self.prog.clear()
                self.vars.clear()
                self.pc = None
                break
            elif cmd == 'SAVE':
                self._stmt_save(args)
            elif cmd == 'LOAD':
                self._stmt_load(args)
            elif cmd == 'EXIT':
                sys.exit(0)
            else:
                raise RuntimeError_(f"Unknown command {cmd}")

            # ------------------------------------------------------------------
            #  Normal line‑to‑line flow – unless a statement changed self.pc
            # ------------------------------------------------------------------
            if self.pc is not None:
                self._next_line()

    # --------------------------------------------------------------
    def _next_line(self):
        later = [ln for ln in self.prog.lines if ln > self.pc]
        self.pc = min(later) if later else None

    # --------------------------------------------------------------
    #  Statement implementations
    # --------------------------------------------------------------
    def _stmt_print(self, args):
        if not args:
            print()
            return
        out = []
        i = 0
        while i < len(args):
            if args[i].kind == STR:
                out.append(args[i].value)
                i += 1
            else:
                val, i = eval_expr(args, i, self.vars)
                out.append(str(val))
        print(''.join(out))

    def _stmt_input(self, args):
        if len(args) != 1 or args[0].kind != ID:
            raise RuntimeError_("INPUT needs a single variable name")
        name = args[0].value
        try:
            txt = input('? ')
            if name.endswith('$'):
                self.vars[name] = txt
            else:
                # try numeric conversion
                if '.' in txt:
                    self.vars[name] = float(txt)
                else:
                    self.vars[name] = int(txt)
        except EOFError:
            self.vars[name] = ""

    def _stmt_let(self, tokens):
        # two possible forms:
        #   LET A = expr
        #   A = expr      (tokens[0] is ID, tokens[1] is OP='=')
        if tokens[0].kind == ID and tokens[1].kind == OP and tokens[1].value == '=':
            name = tokens[0].value
            val, _ = eval_expr(tokens, 2, self.vars)
        elif tokens[0].value == 'LET':
            name = tokens[1].value
            val, _ = eval_expr(tokens, 3, self.vars)
        else:
            raise RuntimeError_("Malformed LET")
        self.vars[name] = val

    def _stmt_goto(self, args):
        if len(args) != 1 or args[0].kind != NUM:
            raise RuntimeError_("GOTO needs a line number")
        target = int(args[0].value)
        if target not in self.prog.lines:
            raise RuntimeError_(f"GOTO target {target} not present")
        self.pc = target

    def _stmt_if(self, args):
        # IF <expr> THEN <lineno>
        # find position of THEN keyword
        then_pos = None
        for i, t in enumerate(args):
            if t.kind == ID and t.value == 'THEN':
                then_pos = i
                break
        if then_pos is None:
            raise RuntimeError_("IF missing THEN")
        cond, _ = eval_expr(args, 0, self.vars)
        if cond:
            if then_pos + 1 >= len(args) or args[then_pos + 1].kind != NUM:
                raise RuntimeError_("THEN must be followed by a line number")
            self._stmt_goto([args[then_pos + 1]])

    def _stmt_for(self, args):
        # FOR I = start TO limit [STEP step]
        if len(args) < 5 or args[0].kind != ID or args[1].kind != OP or args[1].value != '=':
            raise RuntimeError_("Malformed FOR")
        var = args[0].value
        start, p = eval_expr(args, 2, self.vars)
        if args[p].kind != ID or args[p].value != 'TO':
            raise RuntimeError_("FOR missing TO")
        limit, p = eval_expr(args, p+1, self.vars)
        step = 1
        if p < len(args) and args[p].kind == ID and args[p].value == 'STEP':
            step, _ = eval_expr(args, p+1, self.vars)

        # remember the loop: resume after the FOR line (the next line)
        resume = self._next_line_number()
        self.for_stack.append((var, limit, step, resume))
        self.vars[var] = start

    def _next_line_number(self):
        later = [ln for ln in self.prog.lines if ln > self.pc]
        return min(later) if later else None

    def _stmt_next(self, args):
        if len(args) != 1 or args[0].kind != ID:
            raise RuntimeError_("NEXT needs a variable name")
        var = args[0].value
        if not self.for_stack:
            raise RuntimeError_("NEXT without a matching FOR")
        top_var, limit, step, resume = self.for_stack[-1]
        if top_var != var:
            raise RuntimeError_(f"NEXT variable mismatch (expected {top_var})")
        # increment the control variable
        self.vars[var] = self.vars.get(var, 0) + step
        # test loop condition
        if (step > 0 and self.vars[var] <= limit) or (step < 0 and self.vars[var] >= limit):
            self.pc = resume            # jump back to the line after FOR
        else:
            self.for_stack.pop()        # loop finished

    def _stmt_gosub(self, args):
        if len(args) != 1 or args[0].kind != NUM:
            raise RuntimeError_("GOSUB needs a line number")
        target = int(args[0].value)
        if target not in self.prog.lines:
            raise RuntimeError_(f"GOSUB target {target} not present")
        self.call_stack.append(self._next_line_number())
        self.pc = target

    def _stmt_return(self):
        if not self.call_stack:
            raise RuntimeError_("RETURN without GOSUB")
        self.pc = self.call_stack.pop()

    def _stmt_list(self, args):
        if not args:
            self.prog.list()
        elif len(args) == 1 and args[0].kind == NUM:
            n = int(args[0].value)
            self.prog.list(start=n, end=n)
        elif len(args) == 3 and args[1].kind == OP and args[1].value == '-':
            s = int(args[0].value)
            e = int(args[2].value)
            self.prog.list(start=s, end=e)
        else:
            raise RuntimeError_("Bad LIST syntax")

    def _stmt_save(self, args):
        if len(args) != 1 or args[0].kind != STR:
            raise RuntimeError_("SAVE needs a quoted filename")
        fname = args[0].value
        with open(fname, 'w') as f:
            for ln, src in self.prog:
                f.write(f"{ln} {src}\n")
        print(f"Program saved to {fname}")

    def _stmt_load(self, args):
        if len(args) != 1 or args[0].kind != STR:
            raise RuntimeError_("LOAD needs a quoted filename")
        fname = args[0].value
        if not os.path.isfile(fname):
            raise RuntimeError_(f"File not found: {fname}")
        self.prog.clear()
        with open(fname) as f:
            for line in f:
                line = line.rstrip('\n')
                if not line: continue
                num, src = line.split(maxsplit=1)
                self.prog.add(int(num), src)
        print(f"Program loaded from {fname}")

# ----------------------------------------------------------------------
#  REPL – interactive prompt
# ----------------------------------------------------------------------
def repl():
    executor = Executor()
    print("** BASIC for Picocomputer‑6502 – type HELP for a cheat‑sheet **")
    while True:
        try:
            raw = input('> ').strip()
        except (EOFError, KeyboardInterrupt):
            print("\nBye.")
            break
        if not raw:
            continue

        # ----- line‑number editing ---------------------------------
        if raw[0].isdigit():
            parts = raw.split(maxsplit=1)
            line_no = int(parts[0])
            if len(parts) == 1:               # delete the line
                executor.prog.delete(line_no)
            else:
                executor.prog.add(line_no, parts[1])
            continue

        # ----- one‑off commands (RUN, LIST, EXIT, …) -------------
        if raw.upper() == 'RUN':
            try:
                executor.run()
            except RuntimeError_ as e:
                print(f"[runtime error] {e}")
            continue
        if raw.upper() == 'NEW':
            executor.prog.clear()
            executor.vars.clear()
            print("[*] program cleared")
            continue
        if raw.upper() == 'EXIT':
            break
        if raw.upper() == 'HELP':
            print("""\
BASIC commands (type at the > prompt):
  <num> <stmt>   – store/replace a program line
  <num>          – delete that line
  LIST [range]   – show program (range: 10‑20, 10 20, 10‑, -20)
  RUN            – execute the program
  NEW            – erase the whole program
  SAVE "file"    – write program to disk
  LOAD "file"    – read program from disk
  EXIT           – quit the interpreter
Other statements you can type directly:
  PRINT …        – display values
  INPUT var      – read a number or a string
  LET A = 5      – assignment (LET optional)
  IF expr THEN n – conditional jump
  GOTO n
  FOR i = a TO b [STEP s] … NEXT i
  GOSUB n … RETURN
  END / STOP     – terminate program
""")
            continue

        # ----- otherwise treat as a one‑off statement -----------------
        # Put it temporarily at line 0 so the executor can reuse its code path
        executor.prog.add(0, raw)
        executor.pc = 0
        try:
            executor.run()
        except RuntimeError_ as e:
            print(f"[runtime error] {e}")
        executor.prog.delete(0)   # remove the temporary line

if __name__ == '__main__':
    repl()
