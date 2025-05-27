import argparse
import os
import re
import sys

# Module-level Docstring
"""
SHELTRAN to Python Converter (sheltran_to_python.py)

Purpose:
This script provides a best-effort conversion of SHELTRAN (.shl) and some standard
Fortran 77 files into Python (.py) files. SHELTRAN is a structured Fortran dialect 
used in the GIPSY (Groningen Image Processing System). The goal is to automate a 
significant portion of the tedious syntactic translation, allowing developers to 
focus on semantic adjustments and GIPSY-specific library call replacements.

Overview of Conversion Capabilities:
- **SHELTRAN Control Flow:**
    - IF <cond> THEN / ELSEIF <cond> THEN / ELSE / CIF (or ENDIF) to Python if/elif/else.
    - SELECT <expr> CASE <val> / OTHER / CSELECT to Python if/elif/else.
    - WHILE <cond> / XWHILE / CWHILE to Python while/break.
    - REPEAT / XREPEAT / UNTIL <cond> to Python while True/break/if <cond>: break.
    - FOR <var>=<start>,<end>[,<step>] / XFOR / CFOR to Python for var in range(...): / break.
- **SHELTRAN Procedures:**
    - PROC <name> / CPROC to Python def name(): ...
    - PERFORM <name> to Python name()
- **Fortran Program Units:**
    - PROGRAM <name> to `# PROGRAM <name>`.
    - SUBROUTINE <name>(args) to `def <name>(args): # SUBROUTINE <name>`.
    - FUNCTION <name>(args) (with optional type prefix) to `def <name>(args): # FUNCTION <name>`.
    - END statement to `# END (of unit_name)` with appropriate dedent.
- **Assignments and Expressions:**
    - Standard Fortran assignment statements (`LHS = RHS`) are translated.
    - Expressions (RHS and LHS where applicable) are processed to:
        - Convert common Fortran intrinsic functions to Python equivalents (e.g., `SQRT` to `math.sqrt`, `INDEX` to `.find` with caveats, `LEN`, `MOD`, `MIN`, `MAX`, `ICHAR`, `CHAR`, `LOG`, `EXP`, `SIN`, `COS`, etc.).
        - Translate Fortran array accesses (e.g., `A(I,J)`) to Python's 0-based indexing (`A[I-1][J-1]`) with review comments.
        - Convert basic Fortran string slicing (e.g., `S(I:J)`) to Python's `S[I-1:J]` with review comments.
        - Assignments to string slices (e.g., `TEXT(1:5) = '...'`) are commented out with a note on Python string immutability.
- **Basic Fortran/SHELTRAN Statements:**
    - `CALL <subroutine>(args)`: Arguments are translated using expression rules. Alternate returns are commented.
        - Specific GIPSY `CALL` patterns are handled:
            - `CALL INIT` -> `# PROGRAM START ...`
            - `CALL FINIS` -> `# PROGRAM END ...`
            - `CALL ANYOUT(unit, msg)` -> `print(msg_py) # ANYOUT ...`
            - `CALL ERROR(level, msg)` -> `sys.stderr.write(...)`
            - `CALL SETFBLANK(var)` -> `var_py = GIPSY_BLANK` (with `GIPSY_BLANK` definition added).
    - `STOP` to `sys.exit()`.
    - `RETURN` to `return`.
    - `PARAMETER (<name>=<value>)` to Python global constants.
    - `DATA <vars> /<values>/` to Python variable assignments (simplified translation).
- **Declarations (Converted to Comments):**
    - `INTEGER`, `REAL`, `LOGICAL`, `CHARACTER [*N]`, `DOUBLE PRECISION` are commented out as `# DECLARATION: ...`.
    - `DIMENSION A(d1,d2), B(d3)` is commented as `# DECLARATION: DIMENSION ...`.
    - `COMMON /block/ varlist` is commented as `# COMMON /block/ varlist # Fortran COMMON ...`.
    - `EQUIVALENCE (a,b)` is commented as `# EQUIVALENCE (a,b) # Fortran EQUIVALENCE ...`.
- **General Features:**
    - Manages indentation for Python code blocks.
    - Handles SHELTRAN/Fortran comment lines (C, *, N), Eject (E), Fortran directives (F), and Include (I) lines (converted to Python comments).
    - Processes Fortran continuation lines.

Key Limitations and Manual Review Items:
- **Array Indexing:** While basic conversion to 0-based indexing (`var-1`) is done, complex index expressions or indirect effects require careful manual verification. The script adds `# REVIEW ARRAY ACCESS` and `# REVIEW INDEX` comments.
- **GIPSY-Specific Libraries:** Calls to GIPSY libraries (e.g., `GDSINP`, `FIEINI`, `USERIO`) are translated to Python function call syntax (e.g., `GDSINP(...)`) but require an equivalent Python GIPSY environment or manual reimplementation to be functional.
- **Complex I/O:** `FORMAT` statements and most `READ`/`WRITE` statements involving formats or complex I/O lists are commented out and need manual translation to Python's I/O and string formatting capabilities.
- **DATA Statement Complexity:** The `DATA` statement parser is basic and may fail on complex initializations (e.g., repeat factors, implied DO loops, non-literal values).
- **COMMON/EQUIVALENCE:** These are commented out. Logic relying on them for memory layout or global state requires significant refactoring in Python.
- **Function Signatures (Fortran/SHELTRAN):** Argument passing mechanisms (e.g., pass-by-reference implied by Fortran) and how functions return values (especially for Fortran `FUNCTION` type prefixes) need to be manually ensured in Python.
- **Semantic Equivalence:** This script performs primarily syntactic translation. The semantic meaning, numerical precision, and exact behavior of the original code must be thoroughly tested and verified by a developer.
- **Python Reserved Keywords:** The script does not currently check for conflicts between Fortran variable names and Python reserved keywords. These must be manually resolved.

Basic Command-Line Usage:
    python sheltran_to_python.py input_file.shl [-o output_file.py]

Example:
    python sheltran_to_python.py AID.SHL -o aid_converted.py
    python sheltran_to_python.py myprog.shl 
    (Output will be myprog.py in the same directory)
"""

class SheltranToPythonConverter:
    def __init__(self):
        self.indent_level = 0
        self.output_lines = []
        self.imports = set()
        self.procedure_name = None # Used to track current procedure for RETURN statements
        self.current_select_var = None # For SELECT CASE statements
        self.in_procedure = False # Tracks if currently inside a PROC block
        self.gipsy_blank_defined = False # Tracks if GIPSY_BLANK needs to be defined

    def convert_file(self, input_filepath, output_filepath=None):
        if not output_filepath:
            base, _ = os.path.splitext(input_filepath)
            output_filepath = base + ".py"

        print(f"Converting {input_filepath} to {output_filepath}...")

        with open(input_filepath, 'r') as infile:
            sheltran_lines = infile.readlines()

        # Basic continuation line handling (pre-processing)
        # This merges lines marked with a continuation character in column 6.
        processed_lines = self._handle_continuations(sheltran_lines)

        # Process each logical line after handling continuations.
        for line_number, line_content in enumerate(processed_lines):
            self.process_line(line_content.strip(), line_number + 1) # line_number is 0-indexed

        # Assemble the final Python script.
        # Start with any collected imports.
        final_output_list = []
        
        # Add GIPSY_BLANK definition if needed
        if self.gipsy_blank_defined:
            final_output_list.append("GIPSY_BLANK = float('nan') # Placeholder for GIPSY BLANK value. Verify if 'nan' is appropriate.\n\n")

        if self.imports:
            for imp in sorted(list(self.imports)): # Sort for consistent output
                final_output_list.append(f"import {imp}\n")
            if self.imports: # Add a blank line after imports if any were added
                 final_output_list.append("\n")
        
        # Add the main translated code.
        final_output_list.extend(self.output_lines)

        with open(output_filepath, 'w') as outfile:
            outfile.writelines(final_output_list)
        
        print(f"Conversion complete: {output_filepath}")

    def _handle_continuations(self, lines):
        # _handle_continuations: Merges SHELTRAN lines that are continued.
        # A non-blank, non-zero character in column 6 (index 5) indicates a continuation
        # of the previous line. Content from column 7 (index 6) of the continuation line
        # is appended to the current logical line.
        # Assumes typical Fortran fixed-format columns (e.g., content up to col 72).
        
        merged_lines = []
        current_logical_line = ""
        for line_idx, physical_line in enumerate(lines):
            # Check for continuation character in column 6 (index 5)
            # Continuation lines must have something in column 6 that's not space or '0'.
            is_continuation_line = len(physical_line) > 5 and physical_line[5] not in (' ', '0', '\n', '\r')

            if is_continuation_line:
                if not current_logical_line:
                    # This is an orphaned continuation line (starts with continuation mark but no preceding line part).
                    # Or the first line of the file is a continuation mark (unusual).
                    # We'll treat its content (from col 7) as the start of a new logical line.
                    # This might happen if the file snippet starts mid-statement.
                    current_logical_line = physical_line[6:72].rstrip() 
                else:
                    # Add content from column 7 (index 6) up to 72 (index 71) of the continuation line.
                    current_logical_line += physical_line[6:72].rstrip() 
            else:
                # This is not a continuation line, so the previous logical line (if any) is complete.
                if current_logical_line:
                    merged_lines.append(current_logical_line)
                
                # Start a new logical line with the content of the current physical line (up to col 72).
                current_logical_line = physical_line[:72].rstrip()

        # Add any remaining buffered line (the last logical line in the file).
        if current_logical_line:
            merged_lines.append(current_logical_line)
        
        return merged_lines


    def _add_line(self, python_line, original_sheltran_line_for_context=None):
        # Appends a line of generated Python code to the output buffer, with appropriate indentation.
        # original_sheltran_line_for_context is currently unused but could be for debugging or richer comments.
        indent_space = "    " * self.indent_level
        self.output_lines.append(indent_space + python_line + "\n")

    def _decrease_indent(self):
        # Decreases the current indentation level, ensuring it doesn't go below zero.
        if self.indent_level > 0:
            self.indent_level -= 1

    def process_line(self, line_content, line_number):
        # Core method for processing a single logical SHELTRAN line and converting it to Python.
        # line_number is the 1-based number of the logical line in the input .shl file.
        
        # Skip truly empty lines (after continuation handling and stripping whitespace).
        # However, lines with only spaces might be intentional (e.g., inside a FORMAT statement, though we comment those out).
        if not line_content.strip() and not line_content.startswith(" "): 
            return

        original_line_for_comment = line_content # Save for use in comments if no specific rule matches.

        # --- Column 1 Keyword/Comment Checks ---
        # SHELTRAN uses column 1 for special directive characters.
        col1_char = line_content[0] if len(line_content) > 0 else ' '

        # Standard Fortran/SHELTRAN comment lines
        if col1_char == 'C' or col1_char == '*':
            self._add_line(f"# {line_content[1:]}") # Preserve original comment content
            return
        # SHELTRAN 'Note' lines
        if col1_char == 'N': 
            self._add_line(f"# NOTE: {line_content[1:72].strip()}") # Text in cols 2-72
            return
        # SHELTRAN 'Eject' lines (page break for printer)
        if col1_char == 'E': 
            self._add_line(f"# EJECT: {line_content[1:72].strip()} (SHELTRAN EJECT directive)") 
            return
        # SHELTRAN 'Fortran directive' lines (passed to Fortran compiler)
        if col1_char == 'F': 
            self._add_line(f"# FORTRAN DIRECTIVE: {line_content[1:72].strip()} (SHELTRAN F directive)")
            return
        # SHELTRAN 'Include' files
        if col1_char == 'I': 
            filename_to_include = line_content[1:].strip()
            self._add_line(f"# INCLUDE {filename_to_include} (SHELTRAN I directive - manual include/translation needed if it's Python code)")
            return

        # --- Main Code Processing (typically starting column 7) ---
        # SHELTRAN code usually starts in column 7 (index 6), after label (cols 1-5) and continuation (col 6).
        # We strip leading/trailing whitespace from this code part.
        code_part = line_content[6:].strip() if len(line_content) > 6 else ""
        
        if not code_part: # If only a label was present or the rest of the line was blank.
            if line_content.strip(): # If there was content (i.e., a label)
                 self._add_line(f"# Label only: {line_content.strip()}")
            return


        # --- Keyword-based Translation ---
        # The following sections use regex to identify and translate SHELTRAN keywords.
        # Order of these checks can be important for constructs that might partially match.

        # --- Program Unit Keywords (PROGRAM, SUBROUTINE, FUNCTION, END) ---
        # These define the main structure of Fortran/SHELTRAN programs.

        program_match = re.match(r"PROGRAM\s+([A-Za-z_][A-Za-z0-9_]*)", code_part, re.IGNORECASE)
        if program_match:
            program_name = program_match.group(1)
            self._add_line(f"# Fortran PROGRAM {program_name}")
            # A PROGRAM statement itself doesn't usually change indentation for its direct content
            # but sets the context that we are in a main program unit.
            self.procedure_name = program_name # Used for context with the END statement
            self.in_procedure = True # Flag indicating we are inside a defined program unit
            return

        subroutine_match = re.match(r"SUBROUTINE\s+([A-Za-z_][A-Za-z0-9_]*)(?:\s*\(([^)]*)\))?", code_part, re.IGNORECASE)
        if subroutine_match:
            sub_name = subroutine_match.group(1)
            args_str = subroutine_match.group(2) # Argument string, or None if no parentheses
            # Split arguments by comma. Fortran/SHELTRAN args are typically simple identifiers.
            parsed_args = [arg.strip() for arg in args_str.split(',')] if args_str else []
            # Translate to a Python function definition.
            self._add_line(f"def {sub_name}({', '.join(parsed_args)}): # Fortran SUBROUTINE {sub_name}")
            self.indent_level += 1
            self.procedure_name = sub_name # Store name for context with END statement
            self.in_procedure = True      # Set flag indicating we're in a program unit
            return

        # Regex for FUNCTION, allowing for an optional type prefix (e.g., "INTEGER FUNCTION FNAME(...)").
        # The type prefix is matched non-capturingly.
        function_match = re.match(r"(?:[A-Za-z_][A-Za-z0-9_]*\s+)?FUNCTION\s+([A-Za-z_][A-Za-z0-9_]*)(?:\s*\(([^)]*)\))?", code_part, re.IGNORECASE)
        if function_match:
            func_name = function_match.group(1) # Captured function name
            args_str = function_match.group(2)  # Captured argument string, or None
            parsed_args = [arg.strip() for arg in args_str.split(',')] if args_str else []
            # Translate to Python function definition.
            # Note: Fortran function return type (if specified) is ignored here; Python functions return values dynamically.
            self._add_line(f"def {func_name}({', '.join(parsed_args)}): # Fortran FUNCTION {func_name}")
            self.indent_level += 1
            self.procedure_name = func_name # Store name for context with END
            self.in_procedure = True      # Set flag
            return
            
        # Standalone END statement (distinguished from ENDIF, CWHILE etc. by specific regexes for those).
        if re.match(r"END$", code_part, re.IGNORECASE):
            # This END should correspond to a PROGRAM, SUBROUTINE, FUNCTION, or SHELTRAN PROC.
            unit_name_ended = self.procedure_name if self.procedure_name else "PROGRAM/SUBROUTINE/FUNCTION/PROC"
            
            if self.in_procedure: # If we are logically inside a unit that should be ended.
                # Check if the last significant line was a CPROC. If so, CPROC already handled the dedent.
                # This is to avoid double dedent for SHELTRAN PROC...CPROC END sequences.
                # This heuristic might need refinement if CPROC isn't always immediately before END for PROCs.
                is_likely_after_cproc = False
                if self.output_lines:
                    for i in range(len(self.output_lines) -1, -1, -1):
                        prev_line = self.output_lines[i].strip()
                        if prev_line and not prev_line.startswith("#"): # Find last non-comment line
                            if prev_line.endswith("(CPROC)"):
                                is_likely_after_cproc = True
                            break
                
                if not is_likely_after_cproc:
                    self._decrease_indent()
            
            self._add_line(f"# END (of {unit_name_ended})")
            self.procedure_name = None    # Reset context
            self.in_procedure = False     # Reset flag
            return

        # PROCEDURE definition: PROC name -> def name(): (SHELTRAN Specific)
        proc_match = re.match(r"PROC\s+([A-Za-z_][A-Za-z0-9_]*)", code_part, re.IGNORECASE)
        if proc_match:
            self.procedure_name = proc_match.group(1)
            self._add_line(f"def {self.procedure_name}(): # PROC {self.procedure_name}")
            self.indent_level += 1
            self.in_procedure = True # Track that we are inside a PROC block
            return

        # End of PROCEDURE: CPROC (SHELTRAN Specific)
        if re.match(r"CPROC", code_part, re.IGNORECASE):
            if self.in_procedure: 
                self._decrease_indent()
                proc_end_comment = f"# End of PROC {self.procedure_name if self.procedure_name else ''} (CPROC)"
                self._add_line(proc_end_comment)
                # self.procedure_name = None # Keep for context if END follows immediately
                # self.in_procedure = False # Keep for context if END follows immediately
            else:
                self._add_line(f"# Stray CPROC found (no matching PROC): {code_part}")
            return
        
        # STOP statement -> sys.exit()
        if re.match(r"STOP", code_part, re.IGNORECASE):
            self.imports.add("sys") # Ensure 'sys' is imported in the generated file
            self._add_line("sys.exit() # SHELTRAN STOP")
            return

        # RETURN statement
        if re.match(r"RETURN", code_part, re.IGNORECASE):
            if self.in_procedure: # RETURN from a PROC
                 self._add_line("return # SHELTRAN RETURN (from PROC)")
            else: # RETURN at the global level (implies program termination)
                 self.imports.add("sys")
                 self._add_line("# Top-level SHELTRAN RETURN encountered, translating to sys.exit()")
                 self._add_line("sys.exit() # SHELTRAN RETURN (top-level)")
            return

        # PARAMETER statement: PARAMETER (NAME=VALUE, ...) -> NAME = VALUE
        param_match = re.match(r"PARAMETER\s*\((.+)\)", code_part, re.IGNORECASE)
        if param_match:
            self._add_line(f"# SHELTRAN PARAMETER: {code_part}")
            params_str = param_match.group(1)
            # This regex attempts to split 'name=value' pairs, robustly handling some spacing.
            # It might struggle with very complex values if they aren't simple literals/identifiers.
            param_defs = re.findall(r"([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.+?)(?=\s*,\s*[A-Za-z_][A-Za-z0-9_]*\s*=|$)", params_str)
            for name, value in param_defs:
                python_value = value.strip() 
                # Parameters are often uppercase by convention in Fortran/SHELTRAN.
                self._add_line(f"{name.upper()} = {python_value}") 
            return

        # --- Declaration Statements (INTEGER, REAL, DIMENSION, COMMON, etc.) ---
        # These are translated into comments, as Python is dynamically typed and
        # memory management (for COMMON/EQUIVALENCE) is different.

        # Regex for standard Fortran type declarations (INTEGER, REAL, LOGICAL, CHARACTER[*N], DOUBLE PRECISION)
        # Handles CHARACTER, CHARACTER*N, CHARACTER*(N), CHARACTER*(*), CHARACTER* (LEN=...), etc.
        decl_match = re.match(r"(INTEGER|REAL|LOGICAL|CHARACTER(?:\s*\*[\s\d\(\*\)]+)?|DOUBLE\s+PRECISION)\s+(.+)", code_part, re.IGNORECASE)
        if decl_match:
            type_keyword = decl_match.group(1).upper() # e.g., "INTEGER", "CHARACTER*10"
            variable_list_str = decl_match.group(2)    # The list of variables being declared
            self._add_line(f"# DECLARATION: {type_keyword} {variable_list_str} (Fortran type declaration)")
            return

        # DIMENSION statement for array declarations
        dim_match = re.match(r"DIMENSION\s+(.+)", code_part, re.IGNORECASE)
        if dim_match:
            declaration_list_str = dim_match.group(1) # e.g., "A(10), B(5,5)"
            self._add_line(f"# DECLARATION: DIMENSION {declaration_list_str} (Fortran array dimensioning)")
            return

        # COMMON statement for shared memory blocks
        # Regex captures optional block name: COMMON /BLOCK_NAME/ VAR1, VAR2 or COMMON VAR1, VAR2
        common_match = re.match(r"COMMON(?:\s*/([A-Za-z_][A-Za-z0-9_]*)?/)?\s*(.+)", code_part, re.IGNORECASE)
        if common_match:
            block_name_match = common_match.group(1) # Optional block name
            var_list_str = common_match.group(2)     # List of variables in the common block
            block_name_comment = f"/ {block_name_match} / " if block_name_match else " (blank) "
            self._add_line(f"# COMMON {block_name_comment}{var_list_str} # Fortran COMMON block. Requires manual review for Python global/shared state management.")
            return

        # EQUIVALENCE statement for memory aliasing
        equiv_match = re.match(r"EQUIVALENCE\s*\((.+)\)", code_part, re.IGNORECASE)
        if equiv_match:
            equiv_list_str = equiv_match.group(1) # e.g., "(A, B), (C, D(1))"
            self._add_line(f"# EQUIVALENCE ({equiv_list_str}) # Fortran EQUIVALENCE statement. Requires manual review due to Python's different memory model.")
            return

        # DATA statement for initialising variables.
        # This translation is simplified and primarily handles basic cases.
        # Complex DATA statements (e.g., with implied DOs, array sections, repeat counts for structured types)
        # will likely require manual intervention.
        data_match = re.match(r"DATA\s+(.+?)\s*/(.*?)/", code_part, re.IGNORECASE)
        if data_match:
            self._add_line(f"# Fortran DATA Statement: {code_part} (Simplified translation. Manual review needed for arrays, complex types, or repeat factors like N*value).")
            names_str = data_match.group(1).strip()   # Variables list string
            values_str = data_match.group(2).strip()  # Values list string
            
            # Basic splitting by comma. This will not handle complex list structures perfectly.
            names = [name.strip() for name in names_str.split(',')]
            values = [val.strip() for val in values_str.split(',')]

            if len(names) == len(values):
                for name, value in zip(names, values):
                    # Attempt to translate the value part as an expression, in case it's a PARAMETER or simple const expression.
                    translated_value = self._translate_expression(value)
                    self._add_line(f"{name} = {translated_value} # From DATA statement")
            else:
                self._add_line(f"# Could not fully parse DATA statement: {code_part} (Name/value count mismatch or complex structure not supported by simple split).")
            return
            
        # --- Control Flow Structures (SHELTRAN IF, SELECT, LOOPS, etc.) ---

        # IF THEN / ELSEIF THEN / ELSE / CIF (or ENDIF)
        # Regex for IF <condition> THEN, allowing optional parentheses around condition.
        # Also handles simple logical variables like IF VAR THEN.
        if_then_match = re.match(r"IF\s*(?:\((.+)\)|(.+?))\s+THEN", code_part, re.IGNORECASE)
        if if_then_match:
            condition_in_parens = if_then_match.group(1)
            condition_without_parens = if_then_match.group(2)
            actual_condition_str = condition_in_parens if condition_in_parens else condition_without_parens
            
            py_condition = self._translate_condition(actual_condition_str)
            self._add_line(f"if {py_condition}: # IF ({actual_condition_str}) THEN")
            self.indent_level += 1
            return

        # Regex for ELSEIF <condition> THEN.
        elseif_match = re.match(r"ELSEIF\s*(?:\((.+)\)|(.+?))\s+THEN", code_part, re.IGNORECASE)
        if elseif_match:
            self._decrease_indent() # Python's elif is not nested further than the initial if.
            condition_in_parens = elseif_match.group(1)
            condition_without_parens = elseif_match.group(2)
            actual_condition_str = condition_in_parens if condition_in_parens else condition_without_parens

            py_condition = self._translate_condition(actual_condition_str)
            self._add_line(f"elif {py_condition}: # ELSEIF ({actual_condition_str}) THEN")
            self.indent_level += 1
            return

        if re.match(r"ELSE", code_part, re.IGNORECASE):
            self._decrease_indent()
            self._add_line("else: # ELSE")
            self.indent_level += 1
            return

        # CIF or ENDIF marks the end of an IF block.
        if re.match(r"CIF|ENDIF", code_part, re.IGNORECASE):
            self._decrease_indent()
            self._add_line(f"# End of IF block ({code_part.upper()})") # Add original keyword for clarity
            return

        # SELECT CASE structure: SELECT <expr> / CASE <val>[,<val>..] / OTHER / CSELECT
        # Translated to Python if/elif/else.
        select_match = re.match(r"SELECT\s+(.+)", code_part, re.IGNORECASE)
        if select_match:
            expression_str = select_match.group(1).strip()
            # Sanitize the expression string for use as a Python variable name prefix.
            # This helps create a unique temporary variable for the SELECT expression's value.
            var_name_prefix = re.sub(r'[^A-Za-z0-9_]', '_', expression_str) 
            if not var_name_prefix or not (var_name_prefix[0].isalpha() or var_name_prefix[0] == '_'):
                var_name_prefix = "_" + var_name_prefix # Ensure valid start
            var_name_prefix = var_name_prefix.strip('_').replace('__','_') # Clean up

            self.current_select_var = f"{var_name_prefix}_select_val"
            # SHELTRAN's SELECT truncates the expression to an integer.
            self._add_line(f"# Original SHELTRAN SELECT expression: {expression_str}")
            self._add_line(f"{self.current_select_var} = int({expression_str}) # SELECT (value is truncated to int as per SHELTRAN)") 
            return

        case_match = re.match(r"CASE\s+(.+)", code_part, re.IGNORECASE)
        if case_match:
            if self.current_select_var is None: # Should be inside a SELECT block
                self._add_line(f"# ERROR: CASE statement found outside of a SELECT block: {code_part}")
                return
            
            values_str = case_match.group(1) # e.g., "1", "2,3,4", "'A'" (latter not typical for SHELTRAN SELECT)
            # SHELTRAN CASE usually involves one or more integer values.
            case_values = [v.strip() for v in values_str.split(',')]
            conditions = [f"{self.current_select_var} == {val}" for val in case_values]
            py_condition = " or ".join(conditions)
            
            # Determine if this is the first CASE in the SELECT block.
            is_first_case_in_block = False
            if self.output_lines:
                # Look at the last non-comment line to see if it was the SELECT var assignment.
                for i in range(len(self.output_lines) - 1, -1, -1):
                    last_meaningful_line = self.output_lines[i].strip()
                    if last_meaningful_line and not last_meaningful_line.startswith("#"):
                        if last_meaningful_line.startswith(self.current_select_var + " ="):
                            is_first_case_in_block = True
                        break 
            
            if is_first_case_in_block:
                self._add_line(f"if {py_condition}: # CASE {values_str}")
            else: # Subsequent CASE statements become elif
                self._decrease_indent() 
                self._add_line(f"elif {py_condition}: # CASE {values_str}")
            self.indent_level += 1
            return

        # OTHER clause in SELECT CASE
        if re.match(r"OTHER", code_part, re.IGNORECASE) and self.current_select_var:
            self._decrease_indent() 
            self._add_line("else: # OTHER (in SELECT CASE)")
            self.indent_level += 1
            return

        # CSELECT marks the end of a SELECT CASE block.
        if re.match(r"CSELECT", code_part, re.IGNORECASE):
            if self.current_select_var is None:
                self._add_line(f"# ERROR: CSELECT found outside of a SELECT block: {code_part}")
                return
            self._decrease_indent() # For the last CASE or OTHER block
            self._add_line("# End of SELECT block (CSELECT)")
            self.current_select_var = None # Reset state for subsequent SELECT blocks
            return

        # WHILE loop: WHILE (<condition>) or WHILE <condition>
        while_match = re.match(r"WHILE\s*(?:\((.+)\)|(.+?))", code_part, re.IGNORECASE)
        if while_match:
            condition_in_parens = while_match.group(1)
            condition_without_parens = while_match.group(2)
            actual_condition_str = condition_in_parens if condition_in_parens else condition_without_parens

            py_condition = self._translate_condition(actual_condition_str)
            self._add_line(f"while {py_condition}: # WHILE ({actual_condition_str})")
            self.indent_level += 1
            return
        
        # XWHILE: Exit from WHILE loop
        if re.match(r"XWHILE", code_part, re.IGNORECASE):
            self._add_line("break # XWHILE (exit current WHILE loop)")
            return

        # CWHILE: End of WHILE loop
        if re.match(r"CWHILE", code_part, re.IGNORECASE):
            self._decrease_indent()
            self._add_line("# End of WHILE loop (CWHILE)")
            return

        # REPEAT UNTIL loop: REPEAT / ... / UNTIL <condition>
        # Translated to `while True: ... if <condition>: break`
        if re.match(r"REPEAT", code_part, re.IGNORECASE):
            self._add_line("while True: # REPEAT (start of REPEAT-UNTIL loop)")
            self.indent_level += 1
            return

        # XREPEAT: Exit from REPEAT loop
        if re.match(r"XREPEAT", code_part, re.IGNORECASE):
            self._add_line("break # XREPEAT (exit current REPEAT-UNTIL loop)")
            return

        # UNTIL <condition>
        until_match = re.match(r"UNTIL\s*(?:\((.+)\)|(.+?))", code_part, re.IGNORECASE)
        if until_match:
            condition_in_parens = until_match.group(1)
            condition_without_parens = until_match.group(2)
            actual_condition_str = condition_in_parens if condition_in_parens else condition_without_parens
            
            py_condition = self._translate_condition(actual_condition_str)
            # The block for UNTIL's condition is executed *before* the break.
            self._add_line(f"if {py_condition}: # UNTIL ({actual_condition_str})")
            self.indent_level += 1 
            self._add_line("break")
            self._decrease_indent() # Dedent for the if block
            self._decrease_indent() # Dedent for the 'while True' of the REPEAT
            self._add_line("# End of REPEAT-UNTIL block")
            return
            
        # FOR loop: FOR Var=Start,End[,Step]
        # SHELTRAN FOR loops are inclusive of 'End'; Python's range() is exclusive.
        # This requires adjusting 'End' by +1 (or -1 for negative step).
        for_match = re.match(r"FOR\s+([A-Za-z_][A-Za-z0-9_]*)\s*=\s*([^,]+)\s*,\s*([^,]+)(?:\s*,\s*([^,]+))?", code_part, re.IGNORECASE)
        if for_match:
            loop_var = for_match.group(1)
            start_expr = for_match.group(2).strip()
            end_expr_orig = for_match.group(3).strip()
            step_expr_orig = for_match.group(4)
            
            py_end_expr = ""
            is_step_positive = True # Assume positive step unless step is a negative literal

            if step_expr_orig:
                step_expr_orig = step_expr_orig.strip()
                try: # Check if step is a negative integer literal
                    if int(step_expr_orig) < 0:
                        is_step_positive = False
                except ValueError: 
                    # Step is a variable or complex expression; assume positive for adjustment.
                    # A more robust solution might involve runtime checks if step can vary.
                    pass 

            try: # If 'end_expr_orig' is an integer literal
                end_val_int = int(end_expr_orig)
                py_end_expr = str(end_val_int + 1) if is_step_positive else str(end_val_int - 1)
            except ValueError: # 'end_expr_orig' is a variable or complex expression
                py_end_expr = f"{end_expr_orig} + 1" if is_step_positive else f"{end_expr_orig} - 1"
            
            loop_construct_comment = f"# FOR {loop_var}={start_expr},{end_expr_orig}"
            if step_expr_orig:
                loop_construct_comment += f",{step_expr_orig}"
            
            # Add a warning about 1-based vs 0-based indexing if the loop variable is used for array/list access.
            loop_construct_comment += " (Note: SHELTRAN is 1-based for array indices, Python is 0-based. Manual adjustment of indices may be needed.)"

            if step_expr_orig:
                self._add_line(f"for {loop_var} in range({start_expr}, {py_end_expr}, {step_expr_orig}): {loop_construct_comment}")
            else: # Default step is 1
                self._add_line(f"for {loop_var} in range({start_expr}, {py_end_expr}): {loop_construct_comment}")
            self.indent_level += 1
            return

        # XFOR: Exit from FOR loop
        if re.match(r"XFOR", code_part, re.IGNORECASE):
            self._add_line("break # XFOR (exit current FOR loop)")
            return

        # CFOR: End of FOR loop
        if re.match(r"CFOR", code_part, re.IGNORECASE):
            self._decrease_indent()
            self._add_line("# End of FOR loop (CFOR)")
            return
        
        # PERFORM (procedure call without arguments in SHELTRAN)
        perform_match = re.match(r"PERFORM\s+([A-Za-z_][A-Za-z0-9_]*)", code_part, re.IGNORECASE)
        if perform_match:
            proc_name_to_perform = perform_match.group(1)
            self._add_line(f"{proc_name_to_perform}() # PERFORM {proc_name_to_perform}")
            return
            
        # CALL statement (subroutine call, potentially with arguments and alternate returns)
        call_match = re.match(r"CALL\s+([A-Za-z_][A-Za-z0-9_]*)(?:\s*\((.*)\))?", code_part, re.IGNORECASE)
        if call_match:
            subroutine_name_orig = call_match.group(1)
            subroutine_name_upper = subroutine_name_orig.upper()
            raw_args_string = call_match.group(2) # String containing all arguments, or None
            
            # Handle specific GIPSY routines
            if subroutine_name_upper == 'INIT':
                self._add_line(f"# PROGRAM START (Original CALL {subroutine_name_orig})")
                return
            elif subroutine_name_upper == 'FINIS':
                self._add_line(f"# PROGRAM END (Original CALL {subroutine_name_orig})")
                return
            
            sheltran_individual_args = []
            if raw_args_string:
                sheltran_individual_args = [arg.strip() for arg in re.split(r',(?![^()]*\))', raw_args_string)]

            if subroutine_name_upper == 'ANYOUT':
                unit_translated = "None"
                message_translated = "\"\"" # Default to empty string if not enough args
                if len(sheltran_individual_args) >= 1: # GIPSY ANYOUT might sometimes only have message
                    if len(sheltran_individual_args) >= 2:
                        unit_translated = self._translate_expression(sheltran_individual_args[0])
                        message_translated = self._translate_expression(sheltran_individual_args[1])
                    else: # Only one argument, assume it's the message
                        message_translated = self._translate_expression(sheltran_individual_args[0])
                self._add_line(f"print({message_translated}) # ANYOUT (original unit: {unit_translated if unit_translated != 'None' else 'not specified'})")
                return
            elif subroutine_name_upper == 'ERROR':
                level_translated = "1" # Default error level
                message_translated = "\"\""
                if len(sheltran_individual_args) >= 1: # GIPSY ERROR might sometimes only have message
                    if len(sheltran_individual_args) >= 2:
                        level_translated = self._translate_expression(sheltran_individual_args[0])
                        message_translated = self._translate_expression(sheltran_individual_args[1])
                    else: # Only one argument, assume it's the message
                        message_translated = self._translate_expression(sheltran_individual_args[0])
                self.imports.add("sys")
                # Ensure f-string content is properly escaped if message_translated could be complex
                # For now, assuming simple string or variable after translation
                self._add_line(f'sys.stderr.write(f"ERROR (L{{{level_translated}}}): {{{message_translated}}}\\n") # CALL {subroutine_name_orig}')
                return
            elif subroutine_name_upper == 'SETFBLANK':
                if sheltran_individual_args:
                    variable_name_translated = self._translate_expression(sheltran_individual_args[0])
                    self._add_line(f"{variable_name_translated} = GIPSY_BLANK # SETFBLANK. TODO: Verify GIPSY_BLANK definition is appropriate.")
                    self.gipsy_blank_defined = True
                else:
                    self._add_line(f"# CALL {subroutine_name_orig} with no arguments - expected a variable. Manual check needed.")
                return

            # Default CALL processing for other routines
            translated_py_args = []
            alternate_returns_comments = []
            if raw_args_string: # Re-check as sheltran_individual_args might be empty if raw_args_string was None
                for idx, single_arg_str in enumerate(sheltran_individual_args):
                    if single_arg_str.startswith('*'): 
                        alt_return_target = single_arg_str[1:] 
                        comment_text = (f"# Original CALL had alternate return specifier: {single_arg_str} "
                                        f"at effective argument position {idx+1}. "
                                        "Python does not support direct alternate returns via arguments; "
                                        "this may require refactoring using exceptions, status flags, or callbacks.")
                        alternate_returns_comments.append(comment_text)
                    else:
                        translated_arg = self._translate_expression(single_arg_str)
                        translated_py_args.append(translated_arg)
            
            py_call_str = f"{subroutine_name_orig}({', '.join(translated_py_args)}) # CALL {subroutine_name_orig}"
            
            if alternate_returns_comments:
                py_call_str += " " + " ".join(alternate_returns_comments)
            
            self._add_line(py_call_str)
            return

        # --- I/O Statements ---
        # These are often complex and GIPSY-specific. Basic translation for OPEN/CLOSE/REWIND.
        # READ/WRITE/FORMAT are commented out.
        # Ensure these are checked before general assignment parsing.

        # OPEN(UNIT=u, FILE=f, STATUS=s, ERR=err_label, ...)
        open_match = re.match(r"OPEN\s*\((.+)\)", code_part, re.IGNORECASE)
        if open_match:
            params_str_open = open_match.group(1)
            # Simple key=value parsing. Does not handle quoted values with internal commas well.
            # It looks for patterns like "KEY='VALUE'" or "KEY=IDENTIFIER".
            # Corrected regex to capture key and then either quoted or unquoted value
            open_params_tuples = re.findall(r"([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(?:'([^']*)'|([A-Za-z_][A-Za-z0-9_.]+))", params_str_open, re.IGNORECASE)
            
            processed_open_params = {}
            for key, val_quoted, val_unquoted in open_params_tuples:
                 processed_open_params[key.upper()] = f"'{val_quoted}'" if val_quoted else val_unquoted


            unit_spec = processed_open_params.get("UNIT")
            file_spec = processed_open_params.get("FILE")
            status_spec = processed_open_params.get("STATUS", "''").strip("'").lower() 
            err_handler_spec = processed_open_params.get("ERR")

            if unit_spec and file_spec:
                file_handle_var = f"file_handle_{unit_spec}" # Python variable for the file object
                
                # Map SHELTRAN/Fortran STATUS to Python open() modes
                if status_spec == 'old': py_mode = "'r'"  # File must exist
                elif status_spec == 'new': py_mode = "'w'"  # Create new file or truncate existing
                elif status_spec == 'append': py_mode = "'a'" # Append to existing or create new
                elif status_spec == 'scratch': 
                    py_mode = "'w+'" # For temporary files (read/write, created if not exist)
                    self._add_line(f"# NOTE: OPEN STATUS='SCRATCH' for unit {unit_spec}. Python's 'tempfile' module is often better for scratch files.")
                elif status_spec == 'unknown': py_mode = "'r+'" # Typically read/write, file must exist
                else: py_mode = "'r'" # Default if status is unrecognized or absent

                open_statement_py = f"{file_handle_var} = open({file_spec}, mode={py_mode})"
                
                open_context_comment = f"OPEN unit {unit_spec}, file {file_spec}, status '{status_spec}'"
                if err_handler_spec:
                    self.imports.add("sys") # sys.exit might be used by _translate_exit_specifier
                    self._add_line(f"try: # {open_context_comment} with ERR={err_handler_spec}")
                    self.indent_level += 1
                    self._add_line(open_statement_py)
                    self._decrease_indent()
                    self._add_line("except IOError as e:") # Catch I/O errors during open
                    self.indent_level += 1
                    self._add_line(f'print(f"IOError during {open_context_comment}: {{e}}")')
                    self._add_line(self._translate_exit_specifier(err_handler_spec, f"OPEN unit {unit_spec} ERR branch"))
                    self._decrease_indent()
                else:
                    self._add_line(f"{open_statement_py} # {open_context_comment}")
            else: # UNIT or FILE specifier was missing or unparsable
                self._add_line(f"# Could not fully parse OPEN statement (UNIT or FILE missing/unparsable): {code_part}")
            return

        # CLOSE statement: CLOSE(UNIT=u [, STATUS=s, ERR=e]) or simple CLOSE u
        close_params_str = None
        unit_for_close = None
        close_match_params = re.match(r"CLOSE\s*\((.+)\)", code_part, re.IGNORECASE) # CLOSE(<params>)
        if close_match_params:
            close_params_str = close_match_params.group(1)
            unit_search_in_close = re.search(r"UNIT\s*=\s*([A-Za-z_][A-Za-z0-9_]*)", close_params_str, re.IGNORECASE)
            if unit_search_in_close:
                unit_for_close = unit_search_in_close.group(1)
        else: # Simple CLOSE LUN (no parentheses)
            close_match_simple_lun = re.match(r"CLOSE\s+([A-Za-z0-9_]+)", code_part, re.IGNORECASE)
            if close_match_simple_lun:
                unit_for_close = close_match_simple_lun.group(1)
            
        if unit_for_close:
            # Basic translation. STATUS='DELETE' or 'KEEP' in Fortran CLOSE is not directly mapped here.
            # ERR= handling for CLOSE is less common but could be added similarly to OPEN.
            self._add_line(f"file_handle_{unit_for_close}.close() # CLOSE unit {unit_for_close}")
            if close_params_str and "ERR=" in close_params_str.upper():
                 self._add_line(f"# Note: ERR= clause in CLOSE for unit {unit_for_close} was present; manual try/except may be needed if critical.")
            return

        # REWIND statement: REWIND u or REWIND(UNIT=u [, ERR=e])
        rewind_target_str = None
        rewind_match_simple = re.match(r"REWIND\s+([A-Za-z0-9_]+)", code_part, re.IGNORECASE) # REWIND u
        rewind_match_params = re.match(r"REWIND\s*\((.+)\)", code_part, re.IGNORECASE)    # REWIND (...)
        
        unit_for_rewind = None
        err_handler_for_rewind = None

        if rewind_match_params:
            rewind_params_content = rewind_match_params.group(1)
            unit_search_in_rewind = re.search(r"UNIT\s*=\s*([A-Za-z_][A-Za-z0-9_]*)", rewind_params_content, re.IGNORECASE)
            if unit_search_in_rewind:
                unit_for_rewind = unit_search_in_rewind.group(1)
            err_search_in_rewind = re.search(r"ERR\s*=\s*([A-Za-z_][A-Za-z0-9_]*)", rewind_params_content, re.IGNORECASE)
            if err_search_in_rewind:
                err_handler_for_rewind = err_search_in_rewind.group(1)
        elif rewind_match_simple:
            unit_for_rewind = rewind_match_simple.group(1)

        if unit_for_rewind:
            rewind_py_stmt = f"file_handle_{unit_for_rewind}.seek(0) # REWIND unit {unit_for_rewind}"
            rewind_context_comment = f"REWIND unit {unit_for_rewind}"
            if err_handler_for_rewind:
                self.imports.add("sys") 
                self._add_line(f"try: # {rewind_context_comment} with ERR={err_handler_for_rewind}")
                self.indent_level += 1
                self._add_line(rewind_py_stmt)
                self._decrease_indent()
                self._add_line("except IOError as e:") 
                self.indent_level += 1
                self._add_line(f'print(f"IOError during {rewind_context_comment}: {{e}}")')
                self._add_line(self._translate_exit_specifier(err_handler_for_rewind, f"REWIND unit {unit_for_rewind} ERR branch"))
                self._decrease_indent()
            else:
                self._add_line(rewind_py_stmt)
        elif rewind_match_simple or rewind_match_params: # A REWIND keyword was found but parsing failed
            self._add_line(f"# Could not fully parse REWIND statement: {code_part}")
        if rewind_match_simple or rewind_match_params: # Ensure it returns if REWIND was matched
            return


        # FORMAT statement (identified by a label and the FORMAT keyword)
        # These are complex and have no direct, general Python equivalent. Comment them out.
        # The regex looks for a line starting with digits (label), followed by FORMAT keyword.
        # It uses `line_content` because the label is outside `code_part`.
        format_match = re.match(r"^\s*(\d+)\s+FORMAT\s*\(", line_content, re.IGNORECASE) 
        if format_match:
             label_num = format_match.group(1)
             # Include the original line content for context.
             self._add_line(f"# {label_num} FORMAT {line_content[len(label_num):].strip()} (SHELTRAN/Fortran FORMAT statement - not directly translatable to Python. Manual implementation of formatted I/O needed.)")
             return

        # READ/WRITE statements that are not simple OPEN/CLOSE/REWIND.
        # These are often complex, involving unit numbers, format specifiers (labels or strings),
        # variable lists, and potentially ERR=/END= clauses.
        # They are commented out for manual translation.
        io_rw_match = re.match(r"(READ|WRITE)\s*\(", code_part, re.IGNORECASE)
        if io_rw_match:
             io_keyword = io_rw_match.group(1).upper()
             self._add_line(f"# {io_keyword} {code_part} (SHELTRAN {io_keyword} statement - complex I/O. Requires manual translation to Python's I/O and string formatting, especially if using FORMAT statements or I/O lists.)")
             # If it has ERR= or END=, those would also need specific handling.
             if "ERR=" in code_part.upper() or "END=" in code_part.upper():
                 self._add_line(f"# Note: This {io_keyword} statement may contain ERR= or END= clauses requiring try/except or loop control logic in Python.")
             return


        # --- Fortran Assignment Statement Translation (LHS = RHS) ---
        # This is placed after specific keywords (SHELTRAN, CALL, I/O keywords) 
        # but before the final "UNTRANSLATED" fallback.
        
        # Strategy: Find the first '=' that is not inside parentheses to identify the main assignment operator.
        paren_level = 0
        split_index = -1
        # Iterate over code_part to find the correct '=' for splitting LHS and RHS
        for i, char in enumerate(code_part):
            if char == '(':
                paren_level += 1
            elif char == ')':
                paren_level -= 1
            elif char == '=' and paren_level == 0: # Found the main assignment operator
                split_index = i
                break
        
        if split_index != -1: # If a valid assignment operator was found
            lhs_fortran = code_part[:split_index].strip()
            rhs_fortran = code_part[split_index+1:].strip()

            if lhs_fortran and rhs_fortran: # Ensure both LHS and RHS are non-empty
                # Translate RHS using the comprehensive expression translator
                translated_rhs = self._translate_expression(rhs_fortran)
                
                # Translate LHS using the same comprehensive expression translator
                # This handles array accesses A(I) -> A[I_py] and slices S(I:J) -> S[I_py:J_py] on the LHS
                translated_lhs = self._translate_expression(lhs_fortran)

                # Check if the translated LHS represents an assignment to a string slice
                # (e.g., my_string[0:5] = ..., which is illegal for Python strings).
                is_lhs_string_slice = False
                # Find all bracketed parts in the translated LHS (e.g., "[0]", "[0:5]", "[i][0:5]")
                bracket_groups_in_lhs = re.findall(r"(\[[^\]]+\])", translated_lhs)
                if bracket_groups_in_lhs:
                    # Check if the last bracketed group contains a colon, indicating a slice.
                    if ":" in bracket_groups_in_lhs[-1]:
                        is_lhs_string_slice = True
                
                if is_lhs_string_slice:
                    # If LHS is a string slice, comment out the original line and add a note.
                    self._add_line(f"# ORIGINAL: {original_line_for_comment.strip()}")
                    self._add_line(f"# NOTE: Assignment to a string slice. Python strings are immutable.")
                    self._add_line(f"# Manual refactoring needed (e.g., use list of chars or string building).")
                else:
                    # Standard assignment (to a variable or an array element like my_array[i] = ...)
                    self._add_line(f"{translated_lhs} = {translated_rhs}")
                return # Assignment handled (either translated or commented out)

        # --- Fallback for Unrecognized Lines ---
        # If the line was not a comment, directive, SHELTRAN keyword, CALL, recognized I/O, or parseable assignment
        self._add_line(f"# UNTRANSLATED (L{line_number}): {original_line_for_comment.strip()}")

    def _translate_expression(self, expr_str):
        # Translates a Fortran expression string into a Python equivalent.
        # Handles:
        # 1. Fortran-style array accesses (1-based to 0-based).
        # 2. Fortran-style string slicing (1-based to 0-based).
        # 3. Common Fortran intrinsic function calls.
        # Applied iteratively to handle some level of nesting.
        
        # Order of these transformations is crucial:
        # 1. Array Access: `MYARRAY(I, J)` must become `MYARRAY[i_py][j_py]` before other steps
        #    that might misinterpret `(I, J)` as function arguments or a slice.
        # 2. String Slicing: `MYSTRING(I:J)` becomes `MYSTRING[i_py:j_py]`. Must be distinguished
        #    from array access and function calls.
        # 3. Intrinsic Functions: `FUNC(ARG)` becomes `py_func(arg_py)`. Arguments are recursively translated.

        # --- 1. Fortran Array Access: ARRAY(idx1, idx2, ...) -> array[python_idx1][python_idx2]... ---
        # Regex: IDENTIFIER followed by `(` then a sequence of characters that are NOT `stuff_with_colon` then `)`.
        # The `(?:(?!\s*:\s*)[^()])+` part is a non-capturing group that matches one or more characters
        # that are not parentheses, as long as those characters do not form a `slice_content : slice_content` pattern.
        # This helps distinguish `ARRAY(I, J)` from `STRING(I:J)`.
        array_access_pattern = re.compile(r"([A-Za-z_][A-Za-z0-9_]*)\s*\(((?:(?!\s*:\s*)[^()])+)\)")

        def replace_array_access(match_obj):
            # Callback for re.sub to replace Fortran array access with Python 0-based equivalent.
            array_name = match_obj.group(1)
            indices_str = match_obj.group(2)

            # Split indices by comma. Uses regex to avoid splitting commas inside parentheses
            # (e.g., for an argument that is itself a function call like `A(I, FUNC(X,Y))`).
            fortran_indices = [idx.strip() for idx in re.split(r',(?![^()]*\))', indices_str)]
            
            python_indices_parts = [] # Stores translated Python index expressions
            index_comments = []

            for fi in fortran_indices:
                try:
                    # If index is an integer literal, adjust it
                    py_idx_val = int(fi) - 1
                    python_indices_parts.append(str(py_idx_val))
                except ValueError:
                    # If index is a variable or complex expression, translate it recursively
                    # and then append ` - 1` for 0-based indexing.
                    translated_fi = self._translate_expression(fi) 
                    # Add parentheses around the translated expression if it contains operators,
                    # to ensure correct precedence before subtracting 1 (e.g., `(X+Y) - 1`).
                    if any(op in translated_fi for op in ['+', '-', '*', '/', '%']):
                        python_indices_parts.append(f"({translated_fi}) - 1")
                    else:
                        python_indices_parts.append(f"{translated_fi} - 1")
                    index_comments.append(f"# REVIEW INDEX: Verify 0-based subtraction for index '{fi}': {python_indices_parts[-1]}")
            
            # Reconstruct the Python array access string, e.g., array_name[idx1_py][idx2_py]...
            py_access_str = array_name
            for pi_part in python_indices_parts:
                py_access_str += f"[{pi_part}]"
            
            main_comment = "# REVIEW ARRAY ACCESS: Fortran 1-based indexing converted to 0-based."
            full_comment = " ".join([main_comment] + index_comments)
            return f"{py_access_str} {full_comment}"

        # Iteratively apply array access translation
        prev_expr_str_arr = ""
        while expr_str != prev_expr_str_arr:
            prev_expr_str_arr = expr_str
            expr_str = array_access_pattern.sub(replace_array_access, expr_str)

        # --- 2. Fortran String Slicing: VAR(start:end) -> VAR[python_start:python_end] ---
        # Regex: IDENTIFIER `(` `optional_start_expr` `:` `optional_end_expr` `)`.
        # The colon is the key distinguisher for slices.
        # This pattern is applied *after* array access to avoid confusion if an array element
        # is a string that is then sliced, e.g. `CHAR_ARRAY(I)(1:5)`.
        # After array translation: `CHAR_ARRAY[i_py](1:5)`. The slice regex then acts on this.
        slice_pattern = re.compile(
            r"([A-Za-z_][A-Za-z0-9_]*(?:\[[^\]]+\])*)"  # Base variable, possibly already an array access like VAR[i]
            r"\s*\(\s*([^:\)]*?)\s*:\s*([^:\)]*?)\s*\)"   # (start_expr : end_expr)
        )

        def replace_slice(match_obj):
            # Callback for re.sub to replace Fortran string slices.
            var_name_base = match_obj.group(1) # Variable name, potentially with Python array indices already
            start_expr_f = match_obj.group(2).strip()
            end_expr_f = match_obj.group(3).strip()
            
            py_start_expr = ""
            slice_comment = ""

            if start_expr_f: # If start_expr is present
                try:
                    py_start_expr = str(int(start_expr_f) - 1) # Adjust if integer literal
                except ValueError:
                    # If start_expr is a variable or complex expression, translate and comment for review
                    py_start_expr = self._translate_expression(start_expr_f)
                    slice_comment = " # REVIEW SLICE START: Fortran 1-based index; verify translation of start."
            # If start_expr_f is empty, py_start_expr remains empty, meaning slice from beginning.

            # end_expr_f is used directly in Python slice syntax if present.
            # Fortran S(I:J) includes J, Python S[i:j] excludes j. So, Fortran S(I:J) is Python S[i-1:J].
            # No adjustment needed for end_expr if it's a direct variable or number.
            py_end_expr = self._translate_expression(end_expr_f) if end_expr_f else ""
            
            # Construct Python slice syntax
            return f"{var_name_base}[{py_start_expr}:{py_end_expr}]{slice_comment}"
        
        prev_expr_str_slice = ""
        while expr_str != prev_expr_str_slice:
            prev_expr_str_slice = expr_str
            expr_str = slice_pattern.sub(replace_slice, expr_str)


        # --- 3. Fortran Intrinsic Functions & General Function Calls ---
        # Regex to find function calls: FUNCTION_NAME `(` `arguments` `)`.
        # Uses a negative lookbehind `(?<!\[)` to avoid re-matching parts of already translated
        # Python array/slice syntax like `my_array[...]` during iterative processing.
        
        intrinsic_map = {
            # Name: (Python_equivalent, min_args, max_args, needs_math_module, comment)
            'INDEX': ('{0}.find({1})', 2, 2, False, "# NOTE: Fortran INDEX is 1-based, Python find is 0-based. Adjust if result is used numerically."),
            'LEN':   ('len({0})', 1, 1, False, ""), # Assuming it's for CHARACTER strings
            'MOD':   ('({0} % {1})', 2, 2, False, ""),
            'ICHAR': ('ord({0})', 1, 1, False, ""),
            'CHAR':  ('chr({0})', 1, 1, False, ""),
            'FLOAT': ('float({0})', 1, 1, False, ""),
            'INT':   ('int({0})', 1, 1, False, ""),
            'ABS':   ('abs({0})', 1, 1, False, ""),
            'SQRT':  ('math.sqrt({0})', 1, 1, True, ""),
            'LOG':   ('math.log({0})', 1, 1, True, ""),
            'LOG10': ('math.log10({0})', 1, 1, True, ""),
            'EXP':   ('math.exp({0})', 1, 1, True, ""),
            'SIN':   ('math.sin({0})', 1, 1, True, ""),
            'COS':   ('math.cos({0})', 1, 1, True, ""),
            'TAN':   ('math.tan({0})', 1, 1, True, ""),
            'ASIN':  ('math.asin({0})', 1, 1, True, ""),
            'ACOS':  ('math.acos({0})', 1, 1, True, ""),
            'ATAN':  ('math.atan({0})', 1, 1, True, ""),
            'ATAN2': ('math.atan2({0}, {1})', 2, 2, True, ""),
            'MIN':   ('min({args_str})', 1, float('inf'), False, ""), # Handled by arg splitting
            'MAX':   ('max({args_str})', 1, float('inf'), False, ""), # Handled by arg splitting
            'SIGN':  ('math.copysign(float({0}), float({1}))', 2, 2, True, "# SIGN(A,B) -> math.copysign(float(A), float(B))")
        }

        # Regex to find function calls: FUNCTION_NAME followed by optional space and (arguments).
        # This regex is designed to capture the function name and the entire argument string.
        # It's simplified and doesn't handle nested functions perfectly without iterative application or a proper parser.
        # Ensures it doesn't match already processed array/slice syntax like name[...]
        # by requiring the opening parenthesis NOT to be preceded by a square bracket.
        func_pattern = re.compile(r"([A-Za-z_][A-Za-z0-9_]*)\s*(?<!\[)\(([^)]*)\)", re.IGNORECASE)


        def replace_func(match_obj):
            func_name_fortran = match_obj.group(1).upper()
            args_str_fortran = match_obj.group(2)

            # Before processing as a potential intrinsic, check if it's actually an array/slice
            # that was missed or if it's a user-defined function call.
            # This check helps avoid misinterpreting array/slice as function if regex is too greedy.
            if ":" in args_str_fortran and func_name_fortran.upper() not in intrinsic_map: # Likely a slice if colon present and not a known func
                 # This case should ideally be handled by the slice_pattern if it's robust.
                 # If it reaches here, it might be a malformed slice or one that confused previous regex.
                 pass # Let it be handled by slice logic or remain unchanged if not matching slice.

            if func_name_fortran in intrinsic_map:
                py_template, min_args, max_args, needs_math, comment = intrinsic_map[func_name_fortran]
                
                if needs_math:
                    self.imports.add("math")

                # Split arguments by comma, but be careful with nested parentheses (e.g. FUNC(A, B(1,2)))
                # This basic split is a simplification.
                args_fortran = [arg.strip() for arg in re.split(r',(?![^()]*\))', args_str_fortran)] 
                if args_str_fortran.strip() == "": # Handle case of no arguments if string is empty
                    args_fortran = []
                
                if not (min_args <= len(args_fortran) <= max_args):
                    return match_obj.group(0) + f" # ARG COUNT MISMATCH for {func_name_fortran} (expected {min_args}-{max_args}, got {len(args_fortran)})"

                py_args = [self._translate_expression(arg) for arg in args_fortran]

                if func_name_fortran in ['MIN', 'MAX']:
                    return py_template.format(args_str=", ".join(py_args)) + (f" {comment}" if comment else "")
                else:
                    try:
                        return py_template.format(*py_args) + (f" {comment}" if comment else "")
                    except IndexError: 
                        return match_obj.group(0) + f" # TEMPLATE/ARG MISMATCH for {func_name_fortran}"
            
            # If not in intrinsic_map, it could be a user-defined function or an array access not caught by array_access_pattern
            # If it looks like an array access (no colons in args), it might have been missed.
            # However, the array_access_pattern is designed to be more specific.
            # For now, assume it's a user function call and translate its arguments.
            if '(' in match_obj.group(0) and ')' in match_obj.group(0) and not func_name_fortran in intrinsic_map:
                 args_fortran = [arg.strip() for arg in re.split(r',(?![^()]*\))', args_str_fortran)]
                 if args_str_fortran.strip() == "": args_fortran = []
                 py_args = [self._translate_expression(arg) for arg in args_fortran]
                 return f"{func_name_fortran}({', '.join(py_args)})"


            return match_obj.group(0) # Return original if not an intrinsic and not clearly a user func call with args

        prev_expr_str_func = ""
        while expr_str != prev_expr_str_func:
            prev_expr_str_func = expr_str
            expr_str = func_pattern.sub(replace_func, expr_str)
            
        return expr_str

    def _translate_exit_specifier(self, specifier, context_comment=""):
        # Translates SHELTRAN ERR= or END= specifiers like STOP, RETURN, or a procedure name
        # to appropriate Python actions or commented calls.
        # `context_comment` provides information about where this specifier was found (e.g., "OPEN unit 5 ERR branch").
        specifier_upper = specifier.upper()
        py_comment = f"# From SHELTRAN '{specifier}' in {context_comment}" if context_comment else f"# From SHELTRAN '{specifier}'"
        
        if specifier_upper == "STOP":
            self.imports.add("sys") # Ensure sys is imported for sys.exit()
            return f"sys.exit() {py_comment}"
        elif specifier_upper == "RETURN":
            # In SHELTRAN, ERR=RETURN might mean return from current PROC or terminate program if at top level.
            # This translates to a Python 'return'. Developer must ensure this is contextually correct.
            return f"return {py_comment} (Verify context: may need sys.exit() if this was a top-level program return)"
        elif specifier_upper in ["XWHILE", "XREPEAT", "XFOR"]: 
            # These are loop exit specifiers. In an ERR= clause, they imply exiting the current loop upon error.
            # This is difficult to map perfectly without knowing the surrounding loop structure from this local context.
            # 'break' is a common Python equivalent for exiting the innermost loop.
            return f"break {py_comment} (Intended to exit a SHELTRAN loop due to error; verify correct Python loop is exited)"
        else: 
            # Assume 'specifier' is a procedure name (an error handling routine).
            # Translate to a Python function call.
            return f"{specifier}() {py_comment} (Call to error handling procedure)"

    def _translate_condition(self, condition_str):
        # Translates SHELTRAN/Fortran style logical conditions to Python syntax.
        # e.g., ".EQ." to "==", ".AND." to "and", ".TRUE." to "True".
        
        # First, translate expressions within the condition (like function calls or slices)
        condition_str_py = self._translate_expression(condition_str)
        # Order of replacements is important (e.g., ".GE." before ".GT." to avoid partial match).
        # condition_str_py = condition_str.strip() # Already stripped if coming from _translate_expression
        
        # Handle ".NOT." first as it can be combined with other operators.
        # Using word boundaries (\b) might be safer for some ops if variables can contain op names.
        condition_str_py = re.sub(r"\.NOT\.\s*", "not ", condition_str_py, flags=re.IGNORECASE) # Apply to potentially pre-translated string

        # Define replacements for relational and logical operators.
        # Adding spaces around Python operators ensures separation from variable names.
        operator_replacements = {
            r"\.EQ\.": " == ",
            r"\.NE\.": " != ",
            r"\.GE\.": " >= ",
            r"\.GT\.": " > ",
            r"\.LE\.": " <= ",
            r"\.LT\.": " < ",
            r"\.AND\.": " and ",
            r"\.OR\.": " or ",
            # Fortran logical literals (already handled if .TRUE. or .FALSE. were arguments to functions)
            # but good to have them here too for standalone logicals.
            r"\.TRUE\.": " True ",
            r"\.FALSE\.": " False ",
        }
        for shel_op, py_op in operator_replacements.items():
            condition_str_py = re.sub(shel_op, py_op, condition_str_py, flags=re.IGNORECASE)

        # Clean up multiple spaces that might have been introduced by adding spaces around operators.
        condition_str_py = re.sub(r"\s+", " ", condition_str_py).strip()
        
        # Heuristic: If the condition was a simple SHELTRAN logical variable (e.g., IF FOPEN THEN),
        # it might have been passed as "FOPEN". This is usually valid in Python if FOPEN is a boolean.
        # More complex conditions in SHELTRAN often rely on Fortran's operator precedence.
        # Python's precedence is similar for basic arithmetic and logical ops, but explicit
        # parentheses might be needed for clarity or to enforce specific evaluation order,
        # especially if the original SHELTRAN code omitted them relying on Fortran rules.
        # This translation does not currently add extra parentheses beyond those in the original.
        # Example: `A .EQ. B .AND. C .NE. D` becomes `A == B and C != D`. Python's `and`
        # has lower precedence than `==` and `!=`, so this is typically fine.

        return condition_str_py

if __name__ == "__main__":
    # --- Command-Line Argument Parsing ---
    # Allows the script to be run from the command line with input and optional output file paths.
    cli_parser = argparse.ArgumentParser(
        description="Convert SHELTRAN (.shl) and some Fortran 77 files to Python (.py).",
        epilog=(
            "This script provides a best-effort syntactic translation. Manual review and "
            "adjustment of the output Python code is ESSENTIAL.\n\n"
            "Key areas requiring manual attention include:\n"
            "  - Array Indexing: Fortran is 1-based, Python is 0-based. The script attempts\n"
            "    to convert (e.g., A(I) -> A[I-1]), but complex indices need review.\n"
            "    Look for '# REVIEW ARRAY ACCESS' and '# REVIEW INDEX' comments.\n"
            "  - GIPSY-Specific Libraries: Calls to GIPSY routines (e.g., GDS routines, FIEINI)\n"
            "    are translated syntactically but require a Python GIPSY environment or manual\n"
            "    reimplementation.\n"
            "  - I/O Operations: FORMAT statements and most READ/WRITE operations are commented out\n"
            "    and require manual translation to Python's I/O and string formatting.\n"
            "  - DATA Statements: Complex initializations (repeat factors, etc.) may not be fully translated.\n"
            "  - COMMON/EQUIVALENCE: These are commented out; logic relying on them needs refactoring.\n"
            "  - String Operations: Assignments to string slices are commented due to Python's string immutability.\n"
            "  - Semantic Correctness: Verify numerical precision, logical flow, and overall behavior."
        ),
        formatter_class=argparse.RawTextHelpFormatter # To allow newlines in help messages
    )
    cli_parser.add_argument(
        "input_file", 
        help="Path to the input SHELTRAN (.shl) file."
    )
    cli_parser.add_argument(
        "-o", "--output_file", 
        help="Path to the output Python (.py) file (optional).\n"
             "If not provided, output will be '<input_filename_base>.py'."
    )

    args = cli_parser.parse_args()

    # --- Conversion Process ---
    # Instantiate the converter and process the specified file.
    converter_instance = SheltranToPythonConverter()
    converter_instance.convert_file(args.input_file, args.output_file)
