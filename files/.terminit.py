import datetime

import os
import subprocess
import shutil

import os

def clear_screen():
    print('\x1bc\033[3J', end='')

clear_screen()

large = ('~/dragon/cycle.sh', (90, 45))
medium = ('~/.home.txt', (50, 33))
small = ('~/.jb.txt', (25, 10))

columns, rows = shutil.get_terminal_size(fallback=(0, 0))

def display_text(text):
    lines = text.split('\n')
    max_len = max([len(line) for line in lines])

    for _ in range(rows // 2 - len(lines) // 2 + rows % 2):
        print()

    for line in lines:
        for _ in range(columns // 2 - max_len // 2):
            print(' ', end='')
        print(line)

    for _ in range(rows // 2 - len(lines) // 2 - 1 - rows % 2):
        print()

    input()
    clear_screen()

if datetime.datetime.now().hour >= 12:
    if columns < large[1][0] or rows < large[1][1]:
        if columns < medium[1][0] or rows < medium[1][1]:
            if columns < small[1][0] or rows < small[1][1]:
                pass
            else:
                with open(os.path.expanduser(small[0]), 'r') as f:
                    display_text(f.read())
        else:
            with open(os.path.expanduser(medium[0]), 'r') as f:
                display_text(f.read())
    else:
        os.system('zsh ~/dragon/cycle.sh')

print('JetBlack')
