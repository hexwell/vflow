import platform

from argparse import ArgumentParser
from os.path import join, dirname


class Analyzer:
    IMPORTS, EITHER, EXPORTS = 'imports', 'either', 'exports'
    IMPORTS_EITHER= '.'.join((IMPORTS, EITHER))

    UNEXPECTED_INDENT = 'Unexpected indentation in {} block.'
    REIMPORT = "Re-import of variable '{}'."

    def __init__(self, *, comment, base_indent=1, indent=2):
        self.comment = comment
        self.base_indent = base_indent
        self.indent = indent

        self.imports_directive = comment + ' ' * base_indent + 'IMPORTS:'
        self.either_directive = 'Either:'
        self.optionals_directive = comment + ' ' * base_indent + 'Optionals:'
        self.exports_directive = comment + ' ' * base_indent + 'EXPORTS:'

        self.override_modifier = 'override '

        self.variables = set()

    def msg(self, message):
        print(f'{self.file}:{self.no}')
        print(f'{message}')
        print()

    def warning(self, message):
        self.msg(f'WARNING: {message}')

    def error(self, message):
        self.msg(f'ERROR: {message}')
        exit(1)

    def check_indent(self, line):
        return line.startswith(' ' * self.current_indent)

    def assert_indent(self, line):
        if self.check_indent(line):
            return line.lstrip()

        else:
            self.error(Analyzer.UNEXPECTED_INDENT.format(self.mode))

    def parsevar(self, line, export):
        if ':' in line:
            variable, comment = line.split(':')
            variable, comment = variable.strip(), comment.lstrip()

            if not variable:
                self.error(f"Empty variable name.")

            if not export:
                self.warning(f"Comment in import of variable '{variable}'.")

            if not comment:
                self.warning(f"Empty comment for variable '{variable}'.")

        else:
            variable = line

            if export:
                self.warning(f"Variable '{variable}' is missing comment.")

        return variable

    def check_reimport(self, variable):
        if variable in self.imports:
            self.error(Analyzer.REIMPORT.format(variable))

        self.imports.add(variable)

    def analyze(self, file):
        IMPORTS, IMPORTS_EITHER, EXPORTS = self.IMPORTS, self.IMPORTS_EITHER, self.EXPORTS
        comment, base_indent, indent = self.comment, self.base_indent, self.indent
        imports_directive, exports_directive = self.imports_directive, self.exports_directive
        either_directive, optionals_directive = self.either_directive, self.optionals_directive
        override_modifier = self.override_modifier

        self.file = file

        self.mode = ''
        self.current_indent = 0

        self.imports = set()

        either = set()
        check_either = False

        optionals = False

        with open(file) as f:
            for self.no, line in enumerate(f, start=1):
                line = line.strip()

                if self.mode in {IMPORTS, IMPORTS_EITHER, EXPORTS}:
                    blockline = line.lstrip(comment)


                if line == imports_directive:
                    self.mode = IMPORTS
                    self.current_indent = base_indent + indent
                    optionals = False

                elif line == exports_directive:
                    self.mode = EXPORTS
                    self.current_indent = base_indent + indent
                    optionals = False

                elif (self.mode.startswith(IMPORTS) or self.mode == EXPORTS) and (not line or not line.startswith(comment)):
                    self.mode = ''
                    self.current_indent = 0
                    optionals = False

                elif self.mode == IMPORTS:
                    if not blockline: continue

                    if line == optionals_directive:
                        optionals = True
                        continue

                    blockline = self.assert_indent(blockline)

                    if blockline == either_directive:
                        self.mode = IMPORTS_EITHER
                        self.current_indent += indent
                        continue

                    variable = self.parsevar(blockline, export=False)

                    self.check_reimport(variable)

                    action = self.warning if optionals else self.error

                    if variable not in self.variables:
                        action(f"Import of{' optional ' if optionals else ' '}variable '{variable}' not satisfied.")

                elif self.mode == IMPORTS_EITHER:
                    if self.check_indent(blockline):
                        blockline = blockline.lstrip()
                        variable = self.parsevar(blockline, export=False)

                        self.check_reimport(variable)

                        either.add(variable)

                    else:
                        check_either = True
                        self.current_indent -= indent
                        self.mode = IMPORTS

                elif self.mode == EXPORTS:
                    if not blockline: continue

                    blockline = self.assert_indent(blockline)

                    override = False
                    if blockline.startswith(override_modifier):
                        override = True
                        blockline = blockline[len(override_modifier):].lstrip()

                    variable = self.parsevar(blockline, export=True)

                    if variable in self.variables and not override:
                        self.warning(f"Re-declaration of variable '{variable}' without '{override_modifier.strip()}' modifier.")

                    self.variables.add(variable)


                if check_either:
                    check_either = False

                    satisfied = sum(map(self.variables.__contains__, either))

                    if not satisfied:
                        self.error('Import of either variable {} not satisfied.'.format(' / '.join(f"'{v}'" for v in either)))

                    if satisfied > 1:
                        self.error('More than one import of either variable {} satisfied.'.format(' / '.join(f"'{v}'" for v in either)))

                    either = set()

        del self.file, self.mode, self.current_indent, self.no, self.imports


def bash(root):
    SOURCE_CMD = 'source '
    CD_CMD = 'cd '

    analyzer = Analyzer(comment='#')

    analyzer.analyze(root)

    wd = dirname(root)
    with open(root) as f:
        for line in f:
            if line.startswith(SOURCE_CMD):
                module = join(wd, line[len(SOURCE_CMD):].lstrip().strip())

                if platform.system() == "Windows":
                	module = module.replace('/', '\\')

                analyzer.analyze(module)

            elif line.startswith(CD_CMD):
                wd = join(wd, line[len(CD_CMD):].lstrip().strip())


def main():
    langs = {
        'bash': bash
    }

    p = ArgumentParser(description="Static code analyzer checking for variable dependencies")
    p.add_argument('lang', choices=langs.keys(), help='language')
    p.add_argument('root_file')

    args = p.parse_args()

    langs[args.lang](args.root_file)


if __name__ == '__main__':
    main()
