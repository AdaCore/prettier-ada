import os.path

from e3.testsuite.control import YAMLTestControlCreator
from e3.testsuite.driver.classic import TestAbortWithError
from e3.testsuite.driver.diff import DiffTestDriver


class BaseDriver(DiffTestDriver):
    """Base class to provide common test driver helpers."""

    @property
    def test_control_creator(self):
        return YAMLTestControlCreator(
            {
                "windows": self.env.target.os.name == "windows",
                "x86": self.env.target.cpu.bits == 32,
            }
        )

    def set_up(self):
        super().set_up()

        if not isinstance(self.test_env.get("description"), str):
            raise TestAbortWithError(
                'test.yaml: missing "description" field of type string'
            )

        self.valgrind_log_files = []
        """
        List of filenames for Valgrind reports. A non-empty report means that
        Valgrind detected memory issues.
        """

    def valgrind_wrap(self, args):
        """
        If the Valgrind mode is enabled, wrap the given command line to run it
        under Valgrind.
        """
        if self.env.valgrind:
            filename = self.working_dir(
                f"valgrind-report-{len(self.valgrind_log_files)}.txt"
            )
            self.valgrind_log_files.append(filename)
            args = [
                "valgrind",
                "-q",
                "--leak-check=full",
                f"--log-file={filename}"
            ] + args
        return args

    def compute_failures(self):
        result = super().compute_failures()

        # Include non-empty Valgrind reports to the general log and make the
        # test fail if there is at least one that is non-empty.
        has_valgrind_issues = False
        for filename in self.valgrind_log_files:
            if os.path.exists(filename):
                with open(filename) as f:
                    report = f.read()
                if report:
                    has_valgrind_issues = True
                    self.result.log += "\n\nValgrind report:\n"
                    self.result.log += report
        if has_valgrind_issues:
            result.append("Valgrind detected issues")

        return result
