#!/bin/env python

"""
Simple script that converts a WAT program into a format that Haskell recognizes

Usage:
    ./wat_to_str <PATH_TO_WAT_FILE>

    Pipes the result to stdout

"""

import sys
import pathlib

from typing import List


def convert_to_unit(name: str, program: List[str]):
    result = []

    result.append(program[0] + '\\n\\')

    for line in program[1:-1]:
        stripped = line.lstrip(' ')
        leading_spaces = len(line) - len(stripped)
        result.append(leading_spaces*' ' + '\\' + stripped + '\\n\\')

    result.append('\\' + program[-1])

    return f'{name} = parse\n    \"' + '\n    '.join(result).replace('"', '\\"') + '\"'


if __name__ == '__main__':
    path = pathlib.Path(sys.argv[1])

    with path.open('r') as f:
        lines = f.readlines()
        lines = [l.strip('\n') for l in lines]

        print(convert_to_unit('prog', lines))