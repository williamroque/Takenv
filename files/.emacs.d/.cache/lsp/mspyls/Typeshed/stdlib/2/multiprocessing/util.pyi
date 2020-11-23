from typing import Any
import threading

SUBDEBUG = ...  # type: Any
SUBWARNING = ...  # type: Any

def sub_debug(msg, *args): ...
def debug(msg, *args): ...
def info(msg, *args): ...
def sub_warning(msg, *args): ...
def get_logger(): ...
def log_to_stderr(level=None): ...
def get_temp_dir(): ...
def register_after_fork(obj, func): ...

class Finalize:
    def __init__(self, obj, callback, args=..., kwargs=None, exitpriority=None): ...
    def __call__(self, wr=None): ...
    def cancel(self): ...
    def still_active(self): ...

def is_exiting(): ...

class ForkAwareThreadLock:
    def __init__(self): ...

class ForkAwareLocal(threading.local):
    def __init__(self): ...
    def __reduce__(self): ...
