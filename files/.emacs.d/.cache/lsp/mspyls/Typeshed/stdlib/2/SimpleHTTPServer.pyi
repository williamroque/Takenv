# Stubs for SimpleHTTPServer (Python 2)

from typing import Any, AnyStr, IO, Mapping, Optional, Union
import BaseHTTPServer
from StringIO import StringIO

class SimpleHTTPRequestHandler(BaseHTTPServer.BaseHTTPRequestHandler):
    server_version = ...  # type: str
    def do_GET(self) -> None: ...
    def do_HEAD(self) -> None: ...
    def send_head(self) -> Optional[IO[str]]: ...
    def list_directory(self, path: Union[str, unicode]) -> Optional[StringIO]: ...
    def translate_path(self, path: AnyStr) -> AnyStr: ...
    def copyfile(self, source: IO[AnyStr], outputfile: IO[AnyStr]): ...
    def guess_type(self, path: Union[str, unicode]) -> str: ...
    extensions_map = ...  # type: Mapping[str, str]
