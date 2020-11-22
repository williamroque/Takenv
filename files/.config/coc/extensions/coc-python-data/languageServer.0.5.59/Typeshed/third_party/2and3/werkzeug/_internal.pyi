from typing import Any

class _Missing:
    def __reduce__(self): ...

class _DictAccessorProperty:
    read_only = ...  # type: Any
    name = ...  # type: Any
    default = ...  # type: Any
    load_func = ...  # type: Any
    dump_func = ...  # type: Any
    __doc__ = ...  # type: Any
    def __init__(self, name, default=None, load_func=None, dump_func=None, read_only=None, doc=None): ...
    def __get__(self, obj, type=None): ...
    def __set__(self, obj, value): ...
    def __delete__(self, obj): ...

def _easteregg(app=None): ...
