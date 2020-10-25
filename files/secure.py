from pynput import mouse, keyboard
import os

is_finished = False

def lock_screen(*_):
    global is_finished

    keyboard_listener.stop()
    mouse_listener.stop()

    is_finished = True

    os.system('ffmpeg -ss 0.25 -f avfoundation -framerate 30 -i "0" -t 0.5 ~/desktop/perpetrator.jpg')
    os.system('pmset displaysleepnow')

keyboard_listener = keyboard.Listener(on_release=lock_screen)
keyboard_listener.start()

mouse_listener = mouse.Listener(
        on_move=lock_screen,
        on_click=lock_screen,
        on_scroll=lock_screen)
mouse_listener.start()

while not is_finished:
    input()

