import sys
import os


def modify_debug_mode(file_path, enable_debug):
    try:
        with open(file_path, 'r') as file:
            lines = file.readlines()

        in_source_computer_division = False
        debug_line_found = False
        debug_mode_off = "      *                     WITH DEBUGGING MODE"
        debug_mode_on = "                            WITH DEBUGGING MODE"
        for i, line in enumerate(lines):
            if 'SOURCE-COMPUTER.' in line:
                in_source_computer_division = True
            if in_source_computer_division and 'WITH DEBUGGING MODE' in line:
                debug_line_found = True
                if enable_debug and debug_mode_off in line:
                    lines[i] = line.replace(debug_mode_off, debug_mode_on, 1)
                elif not enable_debug and debug_mode_on in line:
                    lines[i] = line.replace(debug_mode_on, debug_mode_off, 1)

        if in_source_computer_division and not debug_line_found:
            new_debug_mode_line = (debug_mode_off + "\n                                               .\n") \
                if not enable_debug \
                else debug_mode_off + "\n                                               .\n"
            for j, line in enumerate(lines):
                if 'SOURCE-COMPUTER.' in line:
                    lines.insert(j + 1, new_debug_mode_line)
                    break

        with open(file_path, 'w') as file:
            file.writelines(lines)

        print(f"Modification réussie pour {file_path}")
    except Exception as e:
        print(f"Erreur lors de la modification de {file_path}: {e}")


def main():
    if len(sys.argv) < 3:
        print("Usage: python script.py <enable|disable> <file1> <file2> ...")
        return

    args = [chaine.lower() for chaine in sys.argv]
    mode = args
    enable_debug = mode[1] == 'enable'

    for file_path in args[2:]:
        if os.path.isfile(file_path):
            modify_debug_mode(file_path, enable_debug)
        else:
            print(f"Fichier non trouvé: {file_path}")


if __name__ == "__main__":
    main()
