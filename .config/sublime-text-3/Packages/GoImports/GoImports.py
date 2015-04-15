# Sublime Text 3 Plugin that integrates goimports with your favorite editor
#
# Author: Lukas Zilka (lukas@zilka.me)
#
import sublime
import sublime_plugin
import subprocess
import io
import os

MY_PATH = os.path.dirname(os.path.realpath(__file__))

def install():
    script = [
        "GOPATH='%s' go get github.com/bradfitz/goimports" % MY_PATH
    ]
    for ln in script:
        p = subprocess.Popen(ln, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        if p.wait():
            print("Error when installing goimports:")
            print(p.stdout.read())
            print(p.stderr.read())

if not os.path.exists(os.path.join(MY_PATH, "bin/goimports")):
    install()

s = sublime.load_settings("GoImports.sublime-settings")


class GoImportsCommand(sublime_plugin.TextCommand):
    def run(self, edit, saving=False):
        # Get the content of the current window from the text editor.
        selection = sublime.Region(0, self.view.size())
        content = self.view.substr(selection)

        # Shove that content down goimports process's throat.
        process = subprocess.Popen([os.path.join(MY_PATH, "bin/goimports")],
                stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        process.stdin.write(bytes(content, 'utf8'))
        process.stdin.close()
        process.wait()

        # Put the result back.
        self.view.replace(edit, selection, process.stdout.read().decode('utf8'))
