import re
from pathlib import Path

from plantuml_tools.plant_uml_generator import plant_uml_svg_generator

INDENT_BLOCK = "\t"
ROOT_FOLDER = "c:/dev/"
PERFORM_REGEX = r"^\s*PERFORM\s+(?P<label1>[\w\d-]+)\s*( THRU\s+(?P<label2>[\w\d-]+)?\s*(\.)?)?( VARYING\s+(?P<variable>[\w\d-]+)\s+FROM\s+(?P<start>[\w\d-]+)\s+BY\s+(?P<increment>[\w\d-]+)\s+UNTIL\s+(?P<condition>.+))?\s*(\.)?$"


def is_commented_line(line: str) -> bool:
    if line.lstrip().startswith("*"):
        return True
    match = re.search("^\s*[UBDG]*\s*(\*)", line)
    if match:   #line[6:7] == "*":
        return True
    return False


def beautify_cobol(cobol_code):
    # Ajouter des indentations pour les sections et les paragraphes
    cobol_code = re.sub(r'(^\s*IDENTIFICATION\s+DIVISION\.)', r'\1', cobol_code, flags=re.MULTILINE)
    cobol_code = re.sub(r'(^\s*PROCEDURE\s+DIVISION\.)', r'\1', cobol_code, flags=re.MULTILINE)
    cobol_code = re.sub(r'(^\s*\d{2}\s+[A-Z\-]+\.)', r'    \1', cobol_code, flags=re.MULTILINE)

    # Ajouter des sauts de ligne avant les mots-clés COBOL
    keywords = ['IF', 'ELSE', 'END-IF', 'PERFORM', 'END-PERFORM', 'MOVE', 'DISPLAY', 'ACCEPT', 'GO TO', 'COMPUTE',
                'EXIT']
    for keyword in keywords:
        cobol_code = re.sub(r'(^\s*' + keyword + r'\b)', r'\1', cobol_code, flags=re.MULTILINE)

    # Ajuster les indentations pour les blocs IF/ELSE
    cobol_code = re.sub(r'(\n\s*ELSE)', r'\n    \1', cobol_code, flags=re.MULTILINE)
    cobol_code = re.sub(r'(\n\s*END-IF)', r'\n    \1', cobol_code, flags=re.MULTILINE)

    return cobol_code


def is_still_if_condition_part(line: str) -> bool:
    """
    :param line:
    :return:
    """
    if starts_with_cobol_instruction(line):
        return False
    if line.lstrip().startswith('*'):
        return True
    keywords = ['AND', 'OR', 'NOT', "=", ">", "<", ">=", "<=", "<>"
        , "IS EQUAL TO", "IS NOT EQUAL TO"
        , "IS GREATER THAN", "IS NOT GREATER THAN"
        , "IS LESS THAN", "IS NOT LESS THAN"
        , "IS GREATER THAN OR EQUAL TO"
        , "IS LESS THAN OR EQUAL TO"
        , "NOT >", "NOT <", "NOT ="]
    for k in keywords:
        match = re.search(rf'^\s*{k}', line)
        if match:
            return True
    return False


def add_label_usage(label_usage: dict, label_name: str, line_num: int):
    if label_name in label_usage.keys():
        label_usage[label_name].append(line_num)
    else:
        label_usage[label_name] = [line_num]


def is_still_perform_part(line: str) -> bool:
    if line.strip() == "" or is_commented_line(line):
        return True
    if starts_with_cobol_instruction(line):
        return False
    keywords = ['THRU', "VARYING", "FROM", "BY", 'UNTIL', "OR", "AND", "NOT", "TIMES"]
    for k in keywords:
        if k == line or k + " " in line or " " + k in line:
            return True
    return False


def parse_if_block(line_num: int, line_indent: int, if_block_string_level: list, instructions: list, label_usage: dict,
                   labels: dict):
    """
    :param if_block_string_level: list of (line number, string code, level of IF)
    :param labels:
    :param line_num:
    :param line_indent:
    :param perform_string:
    :param instructions:
    :param label_usage:
    :return:
    """
    level1_thenpart = ""
    level1_elsepart = ""
    else_part = False
    num_ligne_else = 0
    num_ligne_endif = len(if_block_string_level)
    for line in if_block_string_level:
        if line[1] == 1 and line[0].strip() == "ELSE":
            else_part = True
            continue
        if line[1] >= 1 and not else_part:
            num_ligne_else += 1
            level1_thenpart += line[0] + "\n"
        if line[1] >= 1 and else_part:
            level1_elsepart += line[0] + "\n"
    if_then_lines = level1_thenpart.split('\n')

    # CONDITIONS
    if_condition = "IF ?"
    if_statements = []
    if_statements.append(if_then_lines[0].strip()[3:])
    nb_lines_if = 1
    then_statements = ""
    else_statements = ""
    go_on = True
    print("--> ", line_num)
    while go_on:
        if_line = if_then_lines[nb_lines_if]
        if starts_with_cobol_instruction(if_line):
            go_on = False
            continue
        if (not is_commented_line(if_line)
                and "END-IF" not in if_line
                and not if_line.endswith(".") and len(if_line) > 0):
            if_statements.append(if_line)
        nb_lines_if += 1
    instructions.append((line_num, if_condition + r"\n".join(if_statements) + "?", line_indent))

    # THEN
    if len(if_block_string_level) > 1 and if_block_string_level[-1][0] == "END-IF":
        del if_block_string_level[-1]
    if_block_string = [tup[0] for tup in if_block_string_level]
    then_statements = "\n".join(if_block_string[nb_lines_if:num_ligne_else])
    code = "PROCEDURE DIVISION\n" + then_statements + "\n"
    (instructions, labels) = parse_cobol(code, line_indent, instructions, labels,
                                         label_usage, line_num + nb_lines_if - 2)

    #ELSE
    else_lines = if_block_string_level[num_ligne_else + 1:num_ligne_endif]
    if else_lines:
        if nb_lines_if == 1:
            instructions.append((line_num + num_ligne_else - nb_lines_if + 1, "ELSE", line_indent))
        else:
            instructions.append((line_num + num_ligne_else, "ELSE", line_indent))
        else_block_string = [tup[0] for tup in else_lines]
        else_statements = "\n".join(else_block_string)
        code = "PROCEDURE DIVISION\n" + else_statements + "\n"
        if nb_lines_if == 1:
            (instructions, labels) = parse_cobol(code, line_indent, instructions, labels,
                                                 label_usage, line_num + num_ligne_else - nb_lines_if)
        else:
            (instructions, labels) = parse_cobol(code, line_indent, instructions, labels,
                                                 label_usage, line_num + num_ligne_else - 1)
    instructions.append((num_ligne_endif + line_num - 1, "ENDIF", line_indent))


def parse_call(line_num: int, line_indent: int, call_string: str, instructions: list, label_usage: dict):
    """
    :param line_num:
    :param line_indent:
    :param call_string:
    :param label_usage: IN/OUT
    :param instructions: IN/OUT
    :return:
    """
    # Regex pour les composants de l'instruction CALL
    identifier = r'[A-Za-z][A-Za-z0-9-]*'
    literal = r'\'[^\']*\''
    procedure_pointer = r'[A-Za-z][A-Za-z0-9-]*-PTR'
    function_pointer = r'[A-Za-z][A-Za-z0-9-]*-FUNC'
    parameter = rf'(BY (REFERENCE|CONTENT|VALUE) )?({identifier}|ADDRESS OF {identifier}|LENGTH OF {identifier}|{literal}|OMITTED)'

    # Regex pour l'instruction CALL complète
    call_regex = rf'''
    CALL\s+({identifier}|{literal}|{procedure_pointer}|{function_pointer})
    (\s+USING\s+{parameter}(\s+{parameter})*)?
    (\s+RETURNING\s+{identifier})?
    (\s+ON EXCEPTION\s+.*)?
    (\s+NOT ON EXCEPTION\s+.*)?
    (\s+ON OVERFLOW\s+.*)?
    (\s+NOT ON OVERFLOW\s+.*)?
    \.
    '''

    # Compilation de la regex avec les options de drapeau pour ignorer les espaces et les nouvelles lignes
    call_pattern = re.compile(call_regex, re.VERBOSE)

    matches = call_pattern.finditer(call_string)
    for match in matches:
        print(match.group())


def parse_perform(line_num: int, line_indent: int, perform_string: str, instructions: list, label_usage: dict):
    """
    :param line_indent:
    :param line_num:
    :param label_usage: IN/OUT
    :param instructions: IN/OUT
    :param perform_string:
    :return: (instructions, label_usage)
    """
    pattern = re.compile(PERFORM_REGEX, re.VERBOSE | re.MULTILINE | re.DOTALL)

    # print("parse_perform ->", repr(perform_string))
    match = pattern.match(perform_string.strip())
    if match:
        # print("Nom de la première procédure:", match.group("label1"))
        if match.group("label1"):
            # print("Nom de la deuxième procédure:", match.group("label2"))
            add_label_usage(label_usage, match.group("label1"), line_num)
        if match.group("label2"):
            # print("Nom de la deuxième procédure:", match.group("label2"))
            add_label_usage(label_usage, match.group("label2"), line_num)
        if match.group("variable"):
            # print("Variable:", match.group("variable"))
            # print("Valeur de départ:", match.group("start"))
            # print("Incrément:", match.group("increment"))
            # print("Condition:", match.group("condition"))
            conditions = match.group('condition').replace('\n', ' ')
            instructions.append((line_num, f"PERFORM VARYING {match.group('label1')} -> {match.group('label2')} "
                                           f"VARYING [{match.group('variable')}] FROM [{match.group('start')}] "
                                           f"BY [{match.group('increment')}] WHILE [{conditions}]", line_indent))
            # todo lorsque une ligne de <conditions> commence par "=", il faut rajouter un espace avant pour ne pas que la partie droite soit en gras
        if not match.group("label2") and not match.group("variable"):
            instructions.append((line_num, f"PERFORM SIMPLE {match.group('label1')}", line_indent))
        if match.group("label2") and not match.group("variable"):
            instructions.append((line_num, f"PERFORM THRU {match.group('label1')} -> {match.group('label2')}",
                                 line_indent))
    else:
        raise KeyError(msg="syntax error in PERFORM")


def display_current_line(line_start_number:int, line_num: int, line: str):
    if line == '':
        pass
    # print(" " * line_start_number, line_num, "//" if is_commented_line(line) else "", repr(line))


def parse_cobol(file_content: str, indentation: int, instructions: list, labels: dict, label_usage: dict,
                line_start_number: int) -> (list, dict):
    """"
    découpage en unités lexicales et garniture sémantique des items
            #             line_start_number + line_num + lignes_consommees_en_avance]:  # todo traiter le '-' sur la colonne 7 qui marque la continuité de ligne
            #             # exemple
            #             #                        MOVE  "726726761761762762769769733733210210496496
            #             #       -                      "501501"
    :param line_start_number:
    :param label_usage:
    :param labels:
    :param instructions:
    :param file_content:
    :param indentation:
    """
    lines = file_content.split('\n')
    current_instruction = ""
    is_in_code_part = False
    is_comment = False
    comment = ""
    lignes_consommees_en_avance = 0
    line_num = 0
    for line_num, line in enumerate(lines, start=1):
        display_current_line(line_start_number, line_num, line)
        # ignore les lignes vides
        if line.strip() == "":
            continue
        # ignore les lignes déjà parsées
        if lignes_consommees_en_avance > 0:
            lignes_consommees_en_avance -= 1
            continue

        # traiter les commentaires
        if is_commented_line(line):
            lignes_consommees_en_avance = 0
            comment = line.strip() + "\n"
            while is_commented_line(lines[line_num + lignes_consommees_en_avance]):
                comment += lines[line_num + lignes_consommees_en_avance].strip() + "\n"
                display_current_line(line_start_number, line_num + lignes_consommees_en_avance, lines[line_num + lignes_consommees_en_avance].strip())
                lignes_consommees_en_avance += 1
            instructions.append((line_start_number + line_num,
                                 'COMMENT ' + comment.replace("-", "=").replace("*", "=").replace("#", "="),      #replace("-", "&#45;").replace("*", "&#42;"),
                                 indentation))
            continue

        # ignore any line before
        if "PROCEDURE DIVISION" in line:
            is_in_code_part = True
            continue
        if not is_in_code_part:
            continue

        # Ajouter la ligne actuelle à l'instruction en cours
        current_instruction += INDENT_BLOCK + line.strip()

        if line.lstrip().startswith('CALL '):
            match = re.search(r'\s+CALL\s+"([\w-]+)"\s+USING\s+([\w\s-]+)', line)
            if match:
                params = match.group(2).split()
                if not line.strip().endswith("."):
                    params[-1] = params[-1].strip('. ')
                    lignes_consommees_en_avance = 0
                    while "." not in lines[line_num + lignes_consommees_en_avance]:
                        params.append(lines[line_num + lignes_consommees_en_avance].strip())
                        display_current_line(line_start_number, line_num + lignes_consommees_en_avance,
                                             lines[line_num + lignes_consommees_en_avance].strip())
                        lignes_consommees_en_avance += 1
                    params.append(lines[line_num + lignes_consommees_en_avance].strip('. '))
                    lignes_consommees_en_avance += 1
                instructions.append(
                    (line_start_number + line_num, f"CALL {match.group(1)}({', '.join(params)})", indentation))
            else:  # pas de params
                match = re.search(r'\s+CALL\s+"([\w-]+)"\s', line)
                instructions.append((line_start_number + line_num, f"CALL {match.group(1)}()", indentation))
        if line.lstrip().startswith('PERFORM '):
            lignes_essayees = 0
            perform_string = line
            go_on = True
            while go_on:
                if (line_num + lignes_essayees) >= len(lines):
                    go_on = False
                    continue
                if is_commented_line(lines[line_num + lignes_essayees]):
                    lignes_essayees += 1
                    continue
                if is_still_perform_part(lines[line_num+lignes_essayees]):
                    perform_string += "\n" + lines[line_num + lignes_essayees].strip()
                    if lines[line_num + lignes_essayees].strip().endswith("."):
                        go_on = False
                    lignes_essayees += 1
                    continue
                if lines[line_num + lignes_essayees].strip().endswith("END-PERFORM"):
                    perform_string += "\n" + lines[line_num + lignes_essayees].strip()
                    lignes_essayees += 1
                    go_on = False
                    continue
                if starts_with_cobol_instruction(lines[line_num + lignes_essayees].strip()):
                    go_on = False
                    continue
                perform_string += "\n" + lines[line_num + lignes_essayees].strip()
                if lines[line_num + lignes_essayees].strip().endswith("."):
                    go_on = False
                lignes_essayees += 1
            lignes_consommees_en_avance = lignes_essayees
            display_current_line(line_start_number, line_num + lignes_consommees_en_avance,
                                 perform_string)
            parse_perform(line_start_number + line_num, indentation, perform_string, instructions, label_usage)
        elif line.lstrip().startswith('IF '):
            # print("IF block", line_num)
            level = 1
            if_block = [(lines[line_num - 1].strip(), level)]
            continue_parsing_if = True
            lignes_consommees_en_avance = 0
            in_else_block = False
            while continue_parsing_if:
                # print(line_num - 1 + lignes_consommees_en_avance, line_num, lignes_consommees_en_avance)
                if (line_num + lignes_consommees_en_avance) >= len(lines):
                    continue_parsing_if = False
                    continue
                display_current_line(line_start_number, line_num + lignes_consommees_en_avance,
                                     lines[line_num + lignes_consommees_en_avance].strip())
                if (is_commented_line(lines[line_num + lignes_consommees_en_avance])
                        or len(lines[line_num + lignes_consommees_en_avance].strip()) == 0):
                    if_block.append((lines[line_num + lignes_consommees_en_avance].strip(), level))
                    lignes_consommees_en_avance += 1
                    continue
                # gestion du niveau de bloc - cas des IF imbriqués
                if lines[line_num + lignes_consommees_en_avance].strip().startswith('IF '):
                    in_else_block = False
                    level += 1
                    if_block.append((lines[line_num + lignes_consommees_en_avance].strip(), level))
                elif lines[line_num + lignes_consommees_en_avance].strip() == 'ELSE':
                    if in_else_block:
                        level -= 1
                    if_block.append((lines[line_num + lignes_consommees_en_avance].strip(), level))
                elif (lines[line_num + lignes_consommees_en_avance].strip() == 'END-IF'
                        or lines[line_num + lignes_consommees_en_avance].strip() == 'END-IF.'
                        or lines[line_num + lignes_consommees_en_avance].strip().endswith('.')):
                    if lines[line_num + lignes_consommees_en_avance].strip().endswith('.'): # le "." met un terme à tous les blocks IF
                        line_code = lines[line_num + lignes_consommees_en_avance].strip()
                        lignes_consommees_en_avance += 1  # à faire pour TU "test_parse_cobol_if_consecutifs"
                        if_block.append((line_code[:-1], level))    # la dernière instruction est mise dans le bloc sans le '.' final
                        while level > 1:
                            if_block.append(("END-IF", level))          # et on ajoute un END-IF pour uniformiser
                            level -= 1
                    else:
                        if_block.append((lines[line_num + lignes_consommees_en_avance].strip(), level))
                    in_else_block = False
                    level -= 1
                    if level <= 0:
                        continue_parsing_if = False
                else:   # ajout des instructions
                    if_block.append((lines[line_num + lignes_consommees_en_avance].strip(), level))
                if continue_parsing_if:
                    lignes_consommees_en_avance += 1
            parse_if_block(line_start_number + line_num, indentation + 1, if_block, instructions, label_usage, labels)
            # lignes_consommees_en_avance += 1
        elif line.lstrip().startswith('ELSE '):
            instructions.append((line_start_number + line_num,
                                 f"ERROR : ELSE outside IF block @line{line_start_number + line_num}", 0))
            print(f"ERROR : ELSE outside IF block @line{line_start_number + line_num}")
        elif line.lstrip().startswith('GO ') and 'TO' in line:
            match = re.search(r'\s*GO +TO\s+([\w-]+)', line)
            if match:
                instructions.append((line_start_number + line_num, f"GO TO {match.group(1)}", indentation))
                add_label_usage(label_usage, match.group(1), line_start_number + line_num)
            else:
                print(f"Syntax Error: no label in GOTO at {line_start_number + line_num} --> {line}")
        elif line.lstrip() == 'EXIT PROGRAM.':
            instructions.append((line_start_number + line_num, f"EXIT", indentation))
        elif line.lstrip().startswith('EXIT.') or line.startswith('EXIT '):
            instructions.append((line_start_number + line_num, f"EXIT", indentation))
        elif starts_with_cobol_instruction(line.lstrip()):
            instructions.append((line_start_number + line_num, line.strip(), indentation))
        else:
            match = re.search(r'^\s+([\w-]+)\.', line)
            if match:
                instructions.append((line_start_number + line_num, f"LABEL {match.group(1)}", indentation))
                labels[match.group(1)] = line_start_number + line_num
            else:
                pass  # todo : c'est probablement la ligne précédente qui continue - à analyser
    # # Ajouter la dernière instruction en cours
    # if current_instruction and len(current_instruction) < 200:
    #     instructions.append((line_start_number + line_num, current_instruction.strip(), indentation))
    instructions, labels = ajout_infos_labels(instructions, labels)
    return instructions, label_usage


def ajout_infos_labels(instructions: list, labels: dict) -> (list, dict):
    # ajout des num de lignes des labels
    new_instruction = ""
    for i, (line_num, instruction, line_indent) in enumerate(instructions):
        if instruction.startswith("GO TO"):
            parts = re.search(r'GO TO\s+([^\s:]+)(:(.*))?', instruction.strip())
            label = parts.group(1).strip()
            try:
                new_instruction = f"GO TO {label}:{labels[label]}"
            except KeyError:
                # print(f"ERROR : Label {label} utilisé à la ligne {line_num}, mais non défini")
                new_instruction = f"GO TO {label}:???"
            instructions[i] = (line_num, new_instruction, line_indent)
        elif instruction.startswith("PERFORM SIMPLE"):
            parts = re.search(r'PERFORM SIMPLE\s+(.+)', instruction.strip())
            label = parts.group(1).strip()
            try:
                new_instruction = f"{instruction}:{labels[label]}"
            except KeyError:
                # print(f"ERROR : Label {label} utilisé à la ligne {line_num}, mais non défini")
                new_instruction = f"{instruction}:???"
            instructions[i] = (line_num, new_instruction, line_indent)
        elif instruction.startswith("PERFORM THRU"):
            parts = re.search(r'PERFORM THRU\s+(.+) -> ([^:]+)?(:(.+))?', instruction.strip())
            start_paragraph = parts.group(1).strip()
            end_paragraph = parts.group(2).strip()
            try:
                new_instruction = f"PERFORM THRU {start_paragraph} -> {end_paragraph}:{labels[start_paragraph]}"
            except KeyError:
                # print(f"ERROR : Label {start_paragraph} utilisé à la ligne {line_num}, mais non défini")
                new_instruction = f"PERFORM THRU {start_paragraph} -> {end_paragraph}:???"
            instructions[i] = (line_num, new_instruction, line_indent)
        elif instruction.startswith("PERFORM TIMES"):
            parts = re.search(r'PERFORM TIMES\s+(.+)X\s+(.+)', instruction.strip())
            start_paragraph = parts.group(1).strip()
            times = parts.group(2).strip()
            try:
                new_instruction = f"{instruction}:{labels[start_paragraph]}"
            except KeyError:
                # print(f"ERROR : Label {start_paragraph} utilisé à la ligne {line_num}, mais non défini")
                new_instruction = f"{instruction}:???"
            instructions[i] = (line_num, new_instruction, line_indent)
        elif instruction.startswith("PERFORM VARYING"):
            parts = re.search(r'PERFORM VARYING\s+(.+)->\s+(.+)\s+VARYING.+', instruction.strip())
            paragraph_name = parts.group(1).strip()
            conditions = parts.group(2).strip()
            parts = instruction.split(":")
            try:
                new_instruction = f"{parts[0]}:{labels[paragraph_name]}"
            except KeyError:
                # print(f"ERROR : Label {paragraph_name} utilisé à la ligne {line_num}, mais non défini")
                new_instruction = f"{parts[0]}:???"
            instructions[i] = (line_num, new_instruction, line_indent)
        elif instruction.startswith("PERFORM VARYING"):
            parts = re.search(r'PERFORM VARYING\s+(.+)WHILE\s+(.+)', instruction.strip())
            paragraph_name = parts.group(1).strip()
            conditions = parts.group(2).strip()
            parts = instruction.split(":")
            try:
                new_instruction = f"{parts[0]}:{labels[paragraph_name]}"
            except KeyError:
                # print(f"ERROR : Label {paragraph_name} utilisé à la ligne {line_num}, mais non défini")
                new_instruction = f"{parts[0]}:???"
            instructions[i] = (line_num, new_instruction, line_indent)
    return instructions, labels


def starts_with_cobol_instruction(line: str) -> bool:
    stripped_code_line = line.strip()
    commandes_cobol = ["ACCEPT ", "ADD ", "ALLOCATE ", "BROWSE ", "CICS ", "CLOSE ", "CLOSE CURSOR ", "COMMIT ",
                       "COMPUTE ", "CONTINUE", "DATA DIVISION", "DECLARE CURSOR", "DELETE ", "DELETE ", "DISPLAY ", "DISPLAY ",
                       "DIVIDE ", "EDIT ", "ELSE ", "ENVIRONMENT DIVISION", "EXEC SQL ", "EXIT ", "EXIT.", "FETCH ",
                       "GO ", "IDENTIFICATION DIVISION", "IF ", "INITIALIZE ", "MOVE ", "MULTIPLY ", "NEXT SENTENCE",
                       "OPEN ", "OPEN CURSOR ", "PERFORM ", "PRINT ", "PROCEDURE DIVISION", "READ ", "REPLACING ",
                       "REWRITE ", "START ", "STOP RUN ", "SUBMIT ", "SUBTRACT ", "WRITE "]
    for cmd in commandes_cobol:
        if stripped_code_line.upper().startswith(cmd):
            return True
    return False


def get_plantuml_style() -> str:
    plantuml = "<style>\n"
    plantuml += "activityDiagram {\n"
    plantuml += "  BackgroundColor #E6F7FF\n"
    plantuml += "  BorderColor #3399FF\n"
    plantuml += "  FontColor #003366\n"
    plantuml += "  FontName arial\n"
    plantuml += "\n"
    plantuml += "  diamond {\n"
    plantuml += "    BackgroundColor #FFFFCC\n"
    plantuml += "    LineColor #FFCC00\n"
    plantuml += "    FontColor #666600\n"
    plantuml += "    FontName arial\n"
    plantuml += "    FontSize 15\n"
    plantuml += "  }\n"
    plantuml += "  arrow {\n"
    plantuml += "    FontColor #FF6600\n"
    plantuml += "    FontName arial\n"
    plantuml += "    FontSize 15\n"
    plantuml += "  }\n"
    plantuml += "  partition {\n"
    plantuml += "    LineColor #FF0000\n"
    plantuml += "    FontColor #006600\n"
    plantuml += "    RoundCorner 10\n"
    plantuml += "    BackgroundColor #FFE6E6\n"
    plantuml += "  }\n"
    plantuml += "  note {\n"
    plantuml += "    FontColor #0000FF\n"
    plantuml += "    LineColor #000080\n"
    plantuml += "    BackgroundColor #CCCCFF\n"
    plantuml += "  }\n"
    plantuml += "}\n"
    plantuml += "document {\n"
    plantuml += "   BackgroundColor White\n"
    plantuml += "}\n"
    plantuml += "</style>\n"
    return plantuml


def get_line_number_url(base_url: str, line_num: int, indent: str):
    if base_url != "":
        return f"[[{base_url}#L{line_num} (voir)]]\n"
    else:
        return f"\n{indent}note right: //(l:{line_num})//\n"


def get_line_number_url_goto(line_num: int):
    return f"//(l:{line_num})//"


def generate_plantuml(instructions: list, label_usages: dict,
                      cobol_file="", base_url="", comments_on=True, chapter_name="") -> str:
    # Cette fonction génère un diagramme d'activité PlantUML basé sur le flux logique extrait avec les numéros de ligne
    plantuml = "@startuml\n"
    plantuml += get_plantuml_style()
    if cobol_file:
        plantuml += f"title {cobol_file}\n"
    plantuml += "start\n"
    if chapter_name:
        plantuml += "partition " + chapter_name +"{\n"
    for i, (line_num, instruction, line_indent) in enumerate(instructions):
        indent = INDENT_BLOCK * line_indent
        if instruction.startswith("IF"):
            parts = re.search(r'IF \?(.*)\?', instruction.strip(), re.MULTILINE | re.DOTALL)
            plantuml += f"{indent}if (__IF__ {parts.group(1)} ?) then (yes)\n"
            plantuml += f"{indent + INDENT_BLOCK * 4}{get_line_number_url('', line_num, '')}\n"
        elif instruction == "ELSE":
            plantuml += f"{indent}else (no)\n"
            plantuml += f"{indent + INDENT_BLOCK * 4}{get_line_number_url('', line_num, '')}\n"
        elif instruction == "ENDIF":
            plantuml += f"{indent}endif\n"
        elif instruction.startswith("CALL"):
            parts = re.search(r'CALL\s+([\w-]+)\(([\w\s,-]+)\)', instruction.strip())
            fichier = parts.group(1)
            path = get_gitlab_path_from_file_path(fichier, ROOT_FOLDER, base_url)  # todo paramètre d'application
            if path:
                plantuml += f"{indent}#Gold:CALL {fichier}({parts.group(2)}) [[{path} (voir fichier)]]|\n"
            else:
                plantuml += f"{indent}#Gold:CALL {fichier}({parts.group(2)})|\n"
            plantuml += f"{indent}{get_line_number_url('', line_num, '')}\n"
        elif instruction.startswith("GO TO"):
            parts = re.search(r'GO TO\s+([\w-]+)(:(.+))?', instruction.strip())
            label = parts.group(1)
            #plantuml += f"{indent}#Pink:GO TO {label} - voir ligne {get_line_number_url_goto(parts.group(3))}> {get_line_number_url(base_url, line_num, '')}"
            plantuml += f"{indent}#Pink:GO TO {label} - voir ligne {get_line_number_url_goto(parts.group(3))}> \n"
            # plantuml += f"{indent}{get_line_number_url(base_url, line_num)}\n"
            plantuml += f"{indent}detach\n"
        elif instruction.startswith("PERFORM SIMPLE"):
            parts = re.search(r'PERFORM SIMPLE\s+(.+)(:(.+))?', instruction.strip())
            if parts:
                plantuml += f"{indent}#Pink:{parts.group(1)} - voir ligne {parts.group(3)}; <<procedure>>\n"
            else:
                print("SYNTAX ERROR IN", instruction)
            plantuml += f"{indent}{get_line_number_url('', line_num, '')}\n"
        elif instruction.startswith("PERFORM THRU"):
            parts = re.search(r'PERFORM THRU\s+(.+) -> ([^:]+)?(:(.+))?', instruction.strip())
            if parts:
                plantuml += f"{indent}#Pink:{parts.group(1)} -> {parts.group(2)} - voir ligne {parts.group(3)}; <<procedure>>\n"
            else:
                print("SYNTAX ERROR IN", instruction)
            plantuml += f"{indent}{get_line_number_url('', line_num, '')}\n"
        elif instruction.startswith("PERFORM VARYING"):
            parts = re.search(r'PERFORM VARYING (.+) -> (.+) VARYING ([^:]+)?(:(.+))', instruction.strip())
            if parts:
                plantuml += f"{indent}while (__PERFORM__ {parts.group(3)}) is (true)\n"
                plantuml += f"{indent}{get_line_number_url('', line_num, '')}\n"
                plantuml += f"{indent + INDENT_BLOCK}#Pink:{parts.group(1)} -> {parts.group(2)} - voir ligne {parts.group(5)}; <<procedure>>\n"
                plantuml += f"{indent + INDENT_BLOCK}{get_line_number_url('', line_num, '')}\n"
                plantuml += f"{indent}endwhile\n"
            else:
                print("SYNTAX ERROR IN", instruction)
                plantuml += f"SYNTAX ERROR IN {instruction} at line {line_num}\n"
        elif instruction.startswith("EXIT"):
            plantuml += f"{indent}stop\n"
        elif instruction.startswith("COMMENT"):
            if comments_on:
                comments = instruction[8:].split("\n")
                plantuml += f"{indent}note\n"
                for c in comments:
                    if c.strip() != "":
                        plantuml += f"{indent + INDENT_BLOCK * 4}.{c}\n"
                plantuml += f"{indent}end note\n"
        elif instruction.startswith("LABEL"):
            label_name = instruction[6:]
            plantuml += f"{indent}#palegreen:{label_name}<\n"
            plantuml += f"{indent}{get_line_number_url('', line_num, '')}\n"
            if label_name in label_usages.keys():
                plantuml += f"{indent}note\n"
                plantuml += f"{indent + INDENT_BLOCK * 4}Usages: lines {str(label_usages[label_name])}\n"
                plantuml += f"{indent}end note\n"
        else:
            plantuml += f"{indent}:{str(instruction).rstrip()};\n"
            plantuml += f"{indent}{get_line_number_url('', line_num, '')}\n"

    plantuml += f"stop\n"
    if chapter_name:
        plantuml += "}\n"

    plantuml += "@enduml"

    return plantuml


def get_cobol_file_path(root_folder: str, cobol_name: str):
    root_path = Path(root_folder)
    for path in root_path.rglob(cobol_name + ".cob"):
        return path
    return None


def get_gitlab_path_from_file_path(cobol_name: str, root_folder: str, gitlab_base: str) -> str:
    path = get_cobol_file_path(root_folder, cobol_name)
    if gitlab_base and path:
        domaine = path.parts[2]
        location = "/".join(path.parts[3:])
        url = f"{gitlab_base}/{domaine}/-/tree/livraison/{location}"
        return url
    else:
        return None


def split_cobol(instructions: list) -> list:
    instruction_buckets = []
    bucket_size = 2000
    bucket = []
    for (line_num, instruction, line_indent) in instructions:
        bucket.append((line_num, instruction, line_indent))
        if len(bucket) > bucket_size:
            if line_indent == 1:
                instruction_buckets.append(bucket)
                bucket = []
    instruction_buckets.append(bucket)
    return instruction_buckets


def get_overall_diagram(file_name: str, instruction_chapters: list) -> str:
    res = """
@startuml
start
"""
    for i, chapter in enumerate(instruction_chapters):
        premiere_ligne = chapter[0][0]
        derniere_ligne = chapter[-1][0]
        res += f"partition {file_name} ({i + 1}/{len(instruction_chapters)})"
        res += "{\n"
        again = ""
        res += f"""
          :lignes {premiere_ligne} à {derniere_ligne};
        """
        dependencies = []
        for l in chapter:
            if "CALL " in l[1]:
                parts = re.search(r'CALL\s+([\w-]+)\(([\w\s,-]+)\)', l[1].strip())
                if parts:
                    fichier = parts.group(1)
                    if fichier not in dependencies:
                        dependencies.append(fichier)
                        res += f"{INDENT_BLOCK}split {again}\n"
                        if not again:
                            again = "again"
                        res += f"{INDENT_BLOCK}:{fichier}.cob|\n"
                        res += f"{INDENT_BLOCK}detach\n"
        if again:
            res += f"{INDENT_BLOCK}split again\n"
            res += f"{INDENT_BLOCK}end split\n"
        res += "}\n"
    res += """
end
@enduml
"""
    return res


def translate_cobol_to_plantuml(cobol_file: str):
    # Lire le contenu du fichier COBOL
    # C:\dev\obpa\srcv2\progbc\TP783.cob

    # cobol_file = r"obpa\srcv2\ssprog\REPARG.cob"
    cobol_file = cobol_file.replace("\\", "/")
    with open(rf'{ROOT_FOLDER}{cobol_file}', 'r') as file:
        cobol_content = file.read()

    # Analyser le fichier COBOL pour extraire le flux logique avec les numéros de ligne
    (instructions, labels) = parse_cobol(cobol_content, 1, [], {}, {}, 0)

    instruction_chapters = split_cobol(instructions)
    module_parts = cobol_file.split("/")
    plantuml_diagrams = []
    plantuml_diagrams.append(get_overall_diagram(cobol_file, instruction_chapters))
    for i, instruction_chapter in enumerate(instruction_chapters):
        # Générer le diagramme d'activité PlantUML
        plantuml_diagram = generate_plantuml(instruction_chapter, labels, cobol_file,
                                             base_url=f"http://gitlab.altair.recouv/999/{module_parts[0]}/-/tree/livraison/{'/'.join(module_parts[1:])}",
                                             comments_on=True,
                                             chapter_name=f"Partie {i + 1}/{len(instruction_chapters)}")
        plantuml_diagrams.append(plantuml_diagram)

    # Enregistrer le diagramme d'activité PlantUML dans un fichier
    for i, plantuml_diagram in enumerate(plantuml_diagrams):
        with open(f'activity_diagram_{i}.puml', 'w') as file:
            file.write(plantuml_diagram)
        print(f"Le diagramme d'activité PlantUML a été généré et enregistré dans activity_diagram_{i}.puml.")
        plant_uml_svg_generator(plantuml_diagram, f"activity_diagram_{i}")


if __name__ == "__main__":
    # translate_cobol_to_plantuml(r"obpa\srcv2\ssprog\REPARG.cob")
    # translate_cobol_to_plantuml(r"obpa\srcv2\progbc\TP783.cob")
    # translate_cobol_to_plantuml(r"obpa\srcv2\progbc\TP78A.cob")
    translate_cobol_to_plantuml("cobol_tools\\TESTXX.cob")