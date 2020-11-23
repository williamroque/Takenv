import pytesseract
from PIL import Image

import re
import os
import subprocess
import time
import signal
import sys

signal.signal(signal.SIGINT, lambda *_: sys.exit(0))

while True:
    files = os.listdir('/users/jetblack/desktop')
    try:
        match_screenshots = re.compile(r'^.+\.png$')
        file = '/users/jetblack/desktop/{}'.format(next(filter(lambda f: match_screenshots.match(f), files)))

        text = pytesseract.image_to_string(Image.open(file))

        process = subprocess.Popen('pbcopy', stdin=subprocess.PIPE)
        process.communicate('{}'.format(text.replace('\n', ' ').replace('\r', ' ')).encode('utf-8'))

        os.system('trash "{}"'.format(file))
    except StopIteration:
        print('No screenshots')

    time.sleep(1)
