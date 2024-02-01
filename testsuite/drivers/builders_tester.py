from drivers.base_driver import BaseDriver


class BuildersTestDriver(BaseDriver):
    """
    Driver to run the builders tester.

    This driver executes the builders_tester binary and subsequently verifies
    its output against the expected output in the "test.out" file.
    """

    def set_up(self):
        super().set_up()

    def run(self):
        p = self.shell(
            self.valgrind_wrap(["builders_tester"]),
            analyze_output=True,
        )
        if p.status:
            self.output += ">>>program returned status code {}\n".format(p.status)
