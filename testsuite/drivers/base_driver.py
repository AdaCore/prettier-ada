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
