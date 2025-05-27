import argparse
import os
import re
import sys

# Module-level Docstring
"""
SHELTRAN to Python Converter (sheltran_to_python.py)

Purpose:
This script provides a best-effort conversion of SHELTRAN (.shl) files, 
a structured Fortran dialect used in the GIPSY (Groningen Image Processing System), 
into Python (.py) files. The goal is to automate a significant portion of the 
tedious syntactic translation, allowing developers to focus on semantic adjustments 
and GIPSY-specific library call replacements.

Overview of Conversion Capabilities:
- Translates SHELTRAN control flow structures:
    - IF <cond> THEN / ELSEIF <cond> THEN / ELSE / CIF (or ENDIF) to Python if/elif/else blocks.
    - SELECT <expr> CASE <val> / OTHER / CSELECT to Python if/elif/else blocks using a temporary variable.
    - WHILE <cond> / XWHILE / CWHILE to Python while/break.
    - REPEAT / XREPEAT / UNTIL <cond> to Python while True/break/if <cond>: break.
    - FOR <var>=<start>,<end>[,<step>] / XFOR / CFOR to Python for var in range(...): / break.
- Converts SHELTRAN procedure definitions and calls:
    - PROC <name> / CPROC to Python def name(): ...
    - PERFORM <name> to Python name()
    - CALL <subroutine>(<args>) to Python <subroutine>(<args>), with alternate returns commented.
- Handles basic SHELTRAN statements:
    - STOP to sys.exit().
    - RETURN to return.
    - PARAMETER (<name>=<value>) to Python global constants (NAME = value).
    - DATA <vars> /<values>/ to Python variable assignments (simplified).
- Comments out SHELTRAN-specific or non-translatable elements:
    - Variable declarations (INTEGER, REAL, CHARACTER, LOGICAL, etc.).
    - EQUIVALENCE statements.
    - FORMAT statements.
    - Complex READ/WRITE statements.
- Manages indentation for Python code blocks.
- Handles SHELTRAN comment lines (C, *, N), Eject (E), Fortran directives (F), and Include (I) lines (converted to Python comments).
- Processes SHELTRAN continuation lines.

Key Limitations:
- Array Indexing: SHELTRAN is 1-indexed, while Python is 0-indexed. The script does NOT automatically adjust array accesses (e.g., `ARRAY(N)` in SHELTRAN becomes `ARRAY(N)` in Python, not `ARRAY[N-1]`). This requires careful manual review and adjustment in the generated Python code.
- GIPSY-Specific Libraries: Translation of GIPSY-specific library calls (e.g., `gdsc_grid`, `INITRANS`, user I/O routines) is beyond the scope of this script. These will typically be left as is or commented out and require manual replacement with equivalent Python library calls or custom implementations.
- FORMAT Statements: FORTRAN FORMAT statements are complex and have no direct, simple equivalent in Python for all cases. They are commented out and must be manually translated to Python string formatting or other I/O mechanisms.
- Complex I/O: While basic OPEN, CLOSE, and REWIND are translated (with ERR= handling), complex READ/WRITE statements, especially those involving FORMATs or implied DO loops, are commented out.
- DATA Statement Parsing: The DATA statement parser is basic and may fail on complex values, especially strings containing commas or repeat factors (e.g., `DATA A,B,C /3*0.0/`).
- CALL Argument Parsing: Argument parsing in CALL statements is basic and might not handle all cases correctly, especially with complex expressions or function calls as arguments.
- Implicit Typing and COMMON Blocks: SHELTRAN's implicit typing and COMMON blocks for shared memory are not directly translatable to Python's variable handling and scoping. Declarations are commented out, and logic relying on COMMON blocks will need significant refactoring.
- Function Signatures for PROC: SHELTRAN PROCs can receive arguments via registers or COMMON blocks. The current translation creates parameter-less Python functions. These will need manual adjustment if arguments are expected.
- Semantic Translation: This script performs primarily syntactic translation. The semantic meaning and behavior of the original SHELTRAN code, especially concerning GIPSY library interactions, must be understood and verified by a developer.
- Reserved Keywords: The script does not currently check if a SHELTRAN variable name conflicts with a Python reserved keyword. If such a conflict occurs, manual renaming will be needed.

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

        # PROCEDURE definition: PROC name -> def name():
        proc_match = re.match(r"PROC\s+([A-Za-z_][A-Za-z0-9_]*)", code_part, re.IGNORECASE)
        if proc_match:
            self.procedure_name = proc_match.group(1)
            self._add_line(f"def {self.procedure_name}(): # PROC {self.procedure_name}")
            self.indent_level += 1
            self.in_procedure = True # Track that we are inside a PROC block
            return

        # End of PROCEDURE: CPROC
        if re.match(r"CPROC", code_part, re.IGNORECASE):
            if self.in_procedure: 
                self._decrease_indent()
                proc_end_comment = f"# End of PROC {self.procedure_name if self.procedure_name else ''} (CPROC)"
                self._add_line(proc_end_comment)
                self.procedure_name = None
                self.in_procedure = False
            else:
                # CPROC found without a preceding PROC
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

        # DATA statement: DATA var1, var2 /val1, val2/ -> var1 = val1; var2 = val2
        # This is a highly simplified translation. Fortran DATA statements can be complex,
        # involving arrays, implied DO loops, and repeat counts (e.g., 3*0.0).
        # This version handles only simple lists of variables and corresponding literal values.
        data_match = re.match(r"DATA\s+(.+?)\s*/(.*?)/", code_part, re.IGNORECASE)
        if data_match:
            self._add_line(f"# SHELTRAN DATA Statement: {code_part} (manual review needed for arrays, complex types, or repeat counts)")
            names_str = data_match.group(1).strip()
            values_str = data_match.group(2).strip()
            
            names = [name.strip() for name in names_str.split(',')]
            # The value splitting is very basic and will fail if string literals contain commas
            # or if there are repeat factors (e.g. N*value).
            values = [val.strip() for val in values_str.split(',')] 

            if len(names) == len(values):
                for name, value in zip(names, values):
                    self._add_line(f"{name} = {value}")
            else:
                # Add a comment indicating parsing difficulty.
                self._add_line(f"# Could not fully parse DATA statement: {code_part} (name/value count mismatch or complex value structure)")
            return

        # EQUIVALENCE statement - No direct Python equivalent. Comment out.
        if re.match(r"EQUIVALENCE", code_part, re.IGNORECASE):
            self._add_line(f"# {code_part} (SHELTRAN EQUIVALENCE statement - no direct Python equivalent, requires manual refactoring of memory layout)") 
            return

        # Variable Declarations (LOGICAL, INTEGER, CHARACTER, REAL, DOUBLE PRECISION)
        # Python is dynamically typed, so declarations are not strictly needed. Comment them out for reference.
        # CHARACTER*N length specifier is captured but not directly used in Python type system.
        decl_match = re.match(r"(LOGICAL|INTEGER|CHARACTER(?:\s*\*?\s*\d+)?|REAL|DOUBLE\s+PRECISION)\s+(.+)", code_part, re.IGNORECASE)
        if decl_match:
            var_type = decl_match.group(1).upper() # e.g., "INTEGER"
            var_names_list = decl_match.group(2)   # e.g., "A, B, C(10)"
            self._add_line(f"# {var_type} {var_names_list} (SHELTRAN variable declaration - Python is dynamically typed; array dimensions/types need manual handling)") 
            return

        # --- Control Flow Structures ---

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
            subroutine_name = call_match.group(1)
            args_str_sheltran = call_match.group(2) # String containing all arguments
            
            python_args_list = []
            alternate_returns_comments = []

            if args_str_sheltran:
                # Simple comma-based splitting. This is a known simplification and will NOT correctly parse:
                # - Quoted string arguments containing commas.
                # - Function calls as arguments if those functions themselves take multiple comma-separated arguments.
                sheltran_arg_parts = [arg.strip() for arg in args_str_sheltran.split(',')]
                for idx, single_arg in enumerate(sheltran_arg_parts):
                    if single_arg.startswith('*'): # SHELTRAN alternate return syntax (*label or *keyword)
                        alt_return_target = single_arg[1:]
                        alternate_returns_comments.append(f"# Original CALL had alternate return target *{alt_return_target} at arg position {idx+1}. Python does not support direct alternate returns; this may require refactoring using exceptions or status flags.")
                    else:
                        python_args_list.append(single_arg) # Keep other arguments as they are
            
            py_call_str = f"{subroutine_name}({', '.join(python_args_list)}) # CALL {subroutine_name}"
            if alternate_returns_comments:
                # Append comments about alternate returns.
                py_call_str += " " + " ".join(alternate_returns_comments)
            self._add_line(py_call_str)
            return

        # --- I/O Statements ---
        # These are often complex and GIPSY-specific. Basic translation for OPEN/CLOSE/REWIND.
        # READ/WRITE/FORMAT are commented out.

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


        # Default catch-all: If no specific rule matched the `code_part` (content from col 7 onwards),
        # comment out the original SHELTRAN line, prefixed with its line number for reference.
        self._add_line(f"# UNTRANSLATED (L{line_number}): {original_line_for_comment.strip()}")

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
        # Order of replacements is important (e.g., ".GE." before ".GT." to avoid partial match).
        condition_str_py = condition_str.strip()
        
        # Handle ".NOT." first as it can be combined with other operators.
        # Using word boundaries (\b) might be safer for some ops if variables can contain op names.
        condition_str_py = re.sub(r"\.NOT\.\s*", "not ", condition_str_py, flags=re.IGNORECASE)

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
            # Fortran logical literals
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
        description="Convert SHELTRAN (.shl) files to Python (.py).",
        epilog=(
            "This script provides a best-effort translation and does not handle all SHELTRAN features perfectly.\n"
            "Manual review and adjustment of the output Python code is essential, especially for:\n"
            "- Array indexing (SHELTRAN is 1-based, Python is 0-based).\n"
            "- GIPSY-specific library calls.\n"
            "- FORMAT statements and complex I/O operations.\n"
            "- DATA statements with repeat factors or complex initializations.\n"
            "- COMMON blocks and memory layout dependencies."
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
