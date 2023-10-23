from drivers.base_driver import BaseDriver


class DocumentFormatterDriver(BaseDriver):
    """
    Driver to run the document formatter.

    Usage Instructions:

    1. Place a "test.yaml" file in the test directory with the following keys:
       - description: A description of the test's purpose
       - driver: Must be set to "document_formatter"
       - format_options: An array containing the CLI options to be passed to the
         document_formatter

    2. Add a "doc.json" file to the test directory (or specify a "doc" key in the
       "test.yaml" file with the filename of the JSON document to be formatted).

    3. Include a "test.out" text file in the test directory with the expected results.
       If a "test.out" file is missing, it will be treated as empty.

    This driver executes the document_formatter binary on the "doc.json" file and
    subsequently verifies its output against the expected output in the "test.out" file.
    """

    @property
    def default_process_timeout(self):
        result = 300

        # Tests run almost 40 times slower under valgrind, so increase the
        # TIMEOUT in that case.
        if self.env.valgrind:
            result *= 40

        return result

    def run(self):
        # Some tests expect the script to stop with an error status code: in
        # that case just print it so that the baseline catches it.
        p = self.shell(
            self.valgrind_wrap(
                [
                    "document_formatter",
                    "-D",
                    self.test_env.get("doc", "doc.json"),
                ]
                + self.test_env.get("format_options", [])
            ),
            catch_error=False,
        )
        if p.status:
            self.output += ">>>program returned status code {}\n".format(p.status)
