from __future__ import absolute_import

import logging as _base


CRITICAL = _base.CRITICAL
ERROR = _base.ERROR
WARNING = _base.WARNING
INFO = _base.INFO
VERBOSE = (_base.INFO + _base.DEBUG) / 2
DEBUG = _base.DEBUG

basicConfig = _base.basicConfig
getLogger = _base.getLogger


class Logger(_base.Logger):
    def verbose(self, msg, *args, **kwargs):
        return self.log(VERBOSE, msg, *args, **kwargs)


_base.addLevelName(VERBOSE, 'VERBOSE')
_base.setLoggerClass(Logger)
