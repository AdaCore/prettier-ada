#! /usr/bin/env python

"""
Usage::

    testsuite.py [OPTIONS]

Run the prettier-ada testsuite.
"""

import os
import sys

import e3.testsuite
from drivers.document_formatter_driver import DocumentFormatterDriver
from drivers.serialization import SerializationDriver


class Testsuite(e3.testsuite.Testsuite):
    tests_subdir = "tests"
    test_driver_map = {
        "document_formatter": DocumentFormatterDriver,
        "serialization": SerializationDriver,
    }

    def add_options(self, parser):
        parser.add_argument(
            "--valgrind", action="store_true", help="Run tests under valgrind"
        )
        parser.add_argument(
            "--rewrite",
            "-r",
            action="store_true",
            help="Rewrite test baselines according to current output.",
        )

    def set_up(self):
        super().set_up()

        self.env.valgrind = self.main.args.valgrind
        self.env.rewrite_baselines = self.main.args.rewrite


if __name__ == "__main__":
    sys.exit(Testsuite(os.path.dirname(__file__)).testsuite_main())
