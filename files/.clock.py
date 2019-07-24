import time
import os
import sys
import re
import datetime
import signal

ongoing_utility = 0

def sigint_handler(sig, frame):
    if ongoing_utility == 1:
        print(' Timer cancelled.')
    elif ongoing_utility == 2:
        print(' Alarm cancelled.')
    sys.exit(0)

signal.signal(signal.SIGINT, sigint_handler)

def set_timer(hours, minutes, seconds, message):
    t = int(hours) * 3600 + int(minutes) * 60 + int(seconds)
    global ongoing_utility
    ongoing_utility = 1
    time.sleep(t)
    os.system('say {}'.format(message))

def set_alarm(t, message):
    if re.match('^(0[0-9]|1[0-9]|2[0-3]|[0-9])(:[0-5][0-9]){2}$', t):
        now = datetime.datetime.now()
        parsed_time = datetime.datetime.strptime('{}:{}:{}:{}'.format(t, now.year, now.month, now.day), '%H:%M:%S:%Y:%m:%d')
        delta = parsed_time - now
        if delta > datetime.timedelta(0):
            global ongoing_utility
            ongoing_utility = 2
            while delta > datetime.timedelta(0):
                delta = parsed_time - datetime.datetime.now()
                time.sleep(5)
            os.system('say {}'.format(message))
        else:
            print('Date earlier than now')
    else:
        print('Invalid date')

if __name__ == '__main__':
    if sys.argv[1] == 'timer':
        set_timer(*sys.argv[2:])
    elif sys.argv[1] == 'alarm':
        set_alarm(*sys.argv[2:])
    else:
        print('Invalid command')
