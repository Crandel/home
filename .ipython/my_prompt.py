from IPython.terminal.prompts import Prompts, Token
import os


class MyPrompt(Prompts):

    def in_prompt_tokens(self, cli=None):
        path = os.path.basename(os.getcwd())
        return [
            (Token.Prompt, '['),
            (Token.PromptNum, str(self.shell.execution_count)),
            (Token.Prompt, '] '),
            (Token.PromptNum, '~/' + path),
            (Token.Prompt, ' > '), ]

    def out_prompt_tokens(self):
        return [
            (Token.OutPrompt, '['),
            (Token.OutPromptNum, str(self.shell.execution_count)),
            (Token.OutPrompt, '] < '), ]
