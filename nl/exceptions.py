

class Paradox(Exception):
    """
    """
    def __init__(self, expr1, expr2, messg):
        self.expr1 = expr1
        self.expr2 = expr2
        self.message = messg

    def __str__(self):
        return """Contradiction between \n%s\nand\n%s\n%s""" % (self.expr1,
                                                            self.expr2,
                                                            self.message)


class LnError(Exception):
    """
    """
    def __init__(self, messg):
        self.message = messg

    def __str__(self):
        return self.message


