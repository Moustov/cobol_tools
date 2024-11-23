from unittest import TestCase

from cobol_activity_diagram import parse_if_block


class TestIf(TestCase):
    def test_parse_if_block_3if_1else_imbriques(self):
        """
        correspond au code COBOL
    IF DEBIT-SP < DEBIT-DIF
        MOVE 2 TO VAR1
        IF LNK-PDB-RET = "26"
            MOVE 3 TO VAR1
        ELSE
            MOVE 7 TO VAR1
            IF W-NUM-SP = 0
                MOVE 4 TO VAR1
            ELSE
                MOVE 5 TO VAR1
            END-IF
        END-IF
    ELSE
        MOVE 6 TO VAR1
    END-IF

        :return:
        """
        if_block = [('IF DEBIT-SP < DEBIT-DIF', 1),
                    ('MOVE 2 TO VAR1', 1),
                    ('IF LNK-PDB-RET = "26"', 2),
                    ('MOVE 3 TO VAR1', 2),
                    ('ELSE', 2),
                    ('MOVE 7 TO VAR1', 2),
                    ('IF W-NUM-SP = 0', 3),
                    ('MOVE 4 TO VAR1', 3),
                    ('ELSE', 3),
                    ('MOVE 5 TO VAR1', 3),
                    ('END-IF', 3),
                    ('END-IF', 2),
                    ('ELSE', 1),
                    ('MOVE 6 TO VAR1', 1),
                    ('END-IF', 1)]
        instructions = []
        label_usage = {}
        labels = {}
        parse_if_block(1, 1, if_block, instructions, label_usage, labels)
        expected_instructions = [(1, 'IF ?DEBIT-SP < DEBIT-DIF?', 1),
                                (2, 'MOVE 2 TO VAR1', 1),
                                (3, 'IF ?LNK-PDB-RET = "26"?', 2),
                                (4, 'MOVE 3 TO VAR1', 2),
                                (5, 'ELSE', 2),
                                (6, 'MOVE 7 TO VAR1', 2),
                                (7, 'IF ?W-NUM-SP = 0?', 3),
                                (8, 'MOVE 4 TO VAR1', 3),
                                (9, 'ELSE', 3),
                                (10, 'MOVE 5 TO VAR1', 3),
                                (11, 'ENDIF', 3),
                                (12, 'ENDIF', 2),
                                (13, 'ELSE', 1),
                                (14, 'MOVE 6 TO VAR1', 1),
                                (15, 'ENDIF', 1),
                                 ]
        self.assertEqual(expected_instructions, instructions)

    def test_parse_if_block_if_imbriques_sans_else(self):
        """
        correspond au code COBOL
        IF LK-CONV-1 NOT > LK-CONV-2
            MOVE 0 TO DIFFDATE
            IF A1 NOT = A2
                PERFORM ASUP
                        THRU
                        ASUP-FIN
            END-IF
        END-IF

        :return:
        """
        if_block = [('IF LK-CONV-1 NOT > LK-CONV-2', 1),
                    ('MOVE 0 TO DIFFDATE', 1),
                    ('IF A1 NOT = A2', 2),
                    ('PERFORM         ASUP', 2),
                    ('THRU', 2),
                    ('ASUP-FIN', 2),
                    ('END-IF', 2),
                    ('END-IF', 1)]
        instructions = []
        label_usage = {}
        labels = {}
        parse_if_block(1, 1, if_block, instructions, label_usage, labels)
        expected_instructions = [(1, 'IF ?LK-CONV-1 NOT > LK-CONV-2?', 1),
                                 (2, 'MOVE 0 TO DIFFDATE', 1),
                                 (3, 'IF ?A1 NOT = A2?', 2),
                                 (4, 'PERFORM THRU ASUP -> ASUP-FIN:???', 2),
                                 (7, 'ENDIF', 2),
                                 (8, 'ENDIF', 1)]
        self.assertEqual(expected_instructions, instructions)

    def test_parse_if_block_if_imbriques_sans_else_avec_point(self):
        """
        correspond au code COBOL
        IF LK-CONV-1 NOT > LK-CONV-2
            MOVE 0 TO DIFFDATE
            IF A1 NOT = A2
                PERFORM ASUP
                        THRU
                        ASUP-FIN
            END-IF
        END-IF

        :return:
        """
        if_block = [('IF LK-CONV-1 NOT > LK-CONV-2', 1),
                    ('MOVE 0 TO DIFFDATE', 1),
                    ('IF A1 NOT = A2', 2),
                    ('PERFORM         ASUP', 2),
                    ('THRU', 2),
                    ('ASUP-FIN', 2),
                    ('END-IF', 2),
                    ('END-IF', 1)]
        instructions = []
        label_usage = {}
        labels = {}
        parse_if_block(1, 1, if_block, instructions, label_usage, labels)
        expected_instructions = [(1, 'IF ?LK-CONV-1 NOT > LK-CONV-2?', 1),
                                 (2, 'MOVE 0 TO DIFFDATE', 1),
                                 (3, 'IF ?A1 NOT = A2?', 2),
                                 (4, 'PERFORM THRU ASUP -> ASUP-FIN:???', 2),
                                 (7, 'ENDIF', 2),
                                 (8, 'ENDIF', 1)]
        self.assertEqual(expected_instructions, instructions)


    def test_parse_if_block_if_imbriques_avec_else(self):
        """
        correspond au code COBOL
        IF LK-CONV-1 NOT > LK-CONV-2
            MOVE 0 TO DIFFDATE
            IF A1 NOT = A2
                PERFORM ASUP
                        THRU
                        ASUP-FIN
            END-IF
        END-IF

        :return:
        """
        if_block = [('IF LK-CONV-1 NOT > LK-CONV-2', 1),
                    ('MOVE 0 TO DIFFDATE', 1),
                    ('IF A1 NOT = A2', 2),
                    ('PERFORM         ASUP', 2),
                    ('THRU', 2),
                    ('ASUP-FIN', 2),
                    ('ELSE', 2),
                    ('MOVE 2 TO DIFFDATE', 2),
                    ('END-IF', 2),
                    ('END-IF', 1)]
        instructions = []
        label_usage = {}
        labels = {}
        parse_if_block(1, 1, if_block, instructions, label_usage, labels)
        expected_instructions = [(1, 'IF ?LK-CONV-1 NOT > LK-CONV-2?', 1),
                                 (2, 'MOVE 0 TO DIFFDATE', 1),
                                 (3, 'IF ?A1 NOT = A2?', 2),
                                 (4, 'PERFORM THRU ASUP -> ASUP-FIN:???', 2),
                                 (7, 'ELSE', 2),
                                 (8, 'MOVE 2 TO DIFFDATE', 2),
                                 (9, 'ENDIF', 2),
                                 (10, 'ENDIF', 1)]
        self.assertEqual(expected_instructions, instructions)

    def test_parse_if_block_if_sans_else(self):
        """
        correspond au code COBOL
        IF LK-CONV-1 NOT > LK-CONV-2
            MOVE 0 TO DIFFDATE.

        :return:
        """
        if_block = [('IF LK-CONV-1 NOT > LK-CONV-2', 1),
                    ('MOVE 0 TO DIFFDATE', 1),
                    ('END-IF', 1)]
        instructions = []
        label_usage = {}
        labels = {}
        parse_if_block(1, 1, if_block, instructions, label_usage, labels)
        expected_instructions = [(1, 'IF ?LK-CONV-1 NOT > LK-CONV-2?', 1),
                                 (2, 'MOVE 0 TO DIFFDATE', 1),
                                 (3, 'ENDIF', 1)]
        self.assertEqual(expected_instructions, instructions)

    def test_parse_if_block_if_else(self):
        """
        correspond au code COBOL
        IF LK-CONV-1 NOT > LK-CONV-2
            MOVE 0 TO DIFFDATE
        ELSE
            MOVE 1 TO DIFFDATE
        ENDIF
        :return:
        """
        if_block = [('IF LK-CONV-1 NOT > LK-CONV-2', 1),
                    ('MOVE 0 TO DIFFDATE', 1),
                    ('ELSE', 1),
                    ('MOVE 1 TO DIFFDATE', 1),
                    ('END-IF', 1)]
        instructions = []
        label_usage = {}
        labels = {}
        parse_if_block(1, 1, if_block, instructions, label_usage, labels)
        expected_instructions = [(1, 'IF ?LK-CONV-1 NOT > LK-CONV-2?', 1),
                                 (2, 'MOVE 0 TO DIFFDATE', 1),
                                 (3, 'ELSE', 1),
                                 (4, 'MOVE 1 TO DIFFDATE', 1),
                                 (5, 'ENDIF', 1)]
        print(repr(instructions))
        self.assertEqual(expected_instructions, instructions)

    def test_parse_if_block_plusieurs_lignes_commentaire(self):
        if_block = [('IF LK-CONV-1 NOT > LK-CONV-2', 1),
                    ('*              AND LK-CONV-1 NOT = LK-CONV-2', 1),
                    ('OR LK-CONV-1 NOT = LK-CONV-2', 1),
                    ('MOVE 0 TO DIFFDATE', 1),
                    ('GO TO DEB-FIN', 1),
                    ('ELSE', 1),
                    ('PERFORM         ASUP', 1),
                    ('THRU', 1),
                    ('ASUP-FIN', 1),
                    ('END-IF', 1)]
        instructions = []
        label_usage = {}
        labels = {}
        parse_if_block(1, 1, if_block, instructions, label_usage, labels)
        expected_instructions = [(1, r'IF ?LK-CONV-1 NOT > LK-CONV-2\nOR LK-CONV-1 NOT = LK-CONV-2?', 1),
                                 (4, 'MOVE 0 TO DIFFDATE', 1),
                                 (5, 'GO TO DEB-FIN:[5]', 1),
                                 (6, 'ELSE', 1),
                                 (7, 'PERFORM THRU ASUP -> ASUP-FIN:[7]', 1),
                                 (10, 'ENDIF', 1)]
        print(repr(instructions))
        self.assertEqual(expected_instructions, instructions)

    def test_parse_if_block_plusieurs_5lignes(self):
        if_block = [('IF LK-CONV-1 NOT > LK-CONV-2', 1),
                    ('*              AND LK-CONV-1 NOT = LK-CONV-2', 1),
                    ('*              AND LK-CONV-1 NOT = LK-CONV-3', 1),
                    ('*              AND LK-CONV-1 NOT = LK-CONV-4', 1),
                    ('OR LK-CONV-1 NOT = LK-CONV-2', 1),
                    ('MOVE 0 TO DIFFDATE', 1),
                    ('GO TO DEB-FIN', 1),
                    ('ELSE', 1),
                    ('PERFORM         ASUP', 1),
                    ('THRU', 1),
                    ('ASUP-FIN', 1),
                    ('END-IF', 1)]
        instructions = []
        label_usage = {}
        labels = {}
        parse_if_block(1, 1, if_block, instructions, label_usage, labels)
        expected_instructions = [(1, r'IF ?LK-CONV-1 NOT > LK-CONV-2\nOR LK-CONV-1 NOT = LK-CONV-2?', 1),
                                 (6, 'MOVE 0 TO DIFFDATE', 1),
                                 (7, 'GO TO DEB-FIN:[7]', 1),
                                 (8, 'ELSE', 1),
                                 (9, 'PERFORM THRU ASUP -> ASUP-FIN:[9]', 1),
                                 (12, 'ENDIF', 1)]
        print(repr(instructions))
        self.assertEqual(expected_instructions, instructions)
