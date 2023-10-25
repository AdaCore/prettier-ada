import json

from drivers.base_driver import BaseDriver


class SerializationDriver(BaseDriver):

    @staticmethod
    def canon_json(doc):
        """
        Return a string representation of the given JSON document so that two
        equal documents have the exact same string representation.
        """
        return json.dumps(doc, ensure_ascii=True, indent=2, sort_keys=True)

    def set_up(self):
        super().set_up()

        # Read and canonicalize the input
        with open(self.test_dir("input.json")) as f:
            doc = json.load(f)
        self.canon_input = self.canon_json(doc)

    @property
    def baseline(self):
        # The baseline is the canonicalized input. There is no filename
        # associated since we do not want to rewrite the input.
        return None, self.canon_input, False

    def run(self):
        # Run the test program and canonicalize its output
        p = self.shell(
            self.valgrind_wrap(["json_tester", "input.json"]),
            analyze_output=False,
        )
        doc = json.loads(p.out)
        self.output += self.canon_json(doc)
