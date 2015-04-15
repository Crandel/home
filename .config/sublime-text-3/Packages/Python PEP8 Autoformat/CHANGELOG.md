# CHANGELOG

## Python PEP8 Autoformat

### 2014.10.17-01
  - Fix ST2 [Issue #20](https://bitbucket.org/StephaneBunel/pythonpep8autoformat/issue/20/no-module-named-argparse) by including argparse module in package.
  - Use sublime.packages_path() API to locate package.
  - Upgrade autotpep8 to version 1.1a0

### 2014.10.06-01
  - Fix autopep8 API changes and close [Issue #19](https://bitbucket.org/StephaneBunel/pythonpep8autoformat/issue/19/not-running-attribute-error)

### 2014.09.30-01
  - Upgrade autopep8 to version 1.0.5a0 to fix [Issue #7 Crashes Sublime Text when extra parenthesis is added](https://bitbucket.org/StephaneBunel/pythonpep8autoformat/issue/18/crashes-sublime-text-when-extra). Thx to Christopher Welborn.
  - Upgrade pep8 to version 1.6.0a0.

### 2013.08.27-01
  - FIX typo in .no-sublime-package(s) filename.

### 2013.08.26-01
  - Sublime Text 3 no longer extracts the contents of .sublime-package files by default, and the fact that Python PEP8 Autoformat needs to read files from disk, the only viable install method was manual. To fix this, .no-sublime-packages file was added as suggested by [Annonymous][issue15]. Thanks!

### 2013.08.25-01
  - Upgrade autopep8 to version 0.9.4a0. This raised an import issue which broke the plug-in with ST2 (import io). autopep8 was modified to import StringIO as io when import io fail.

### 2013.06.13-01
  - Python PEP8 Autoformat must be unzipped to allow it to access files. This is not the default behavior under ST3. Right now (under ST3) the package must be installed manually.

### 2013.06.12-01
  - FIX bad interpretation of 'select' and 'ignore' settings.
  - Upgrade autopep8 to version 0.9.2a0

### 2013.05.09-02
  - Upgrade autopep8 to version 0.9
  
### 2013.05.09-01
  - FIX error parsing "ignore" and "select" settings.
  - Changes misinterpreted "aggressive" settings from boolean to counter.
  - ADD new boolean setting "list-fixes".

### 2013.03.25-02
  - FIX [Issue #7 Does not work with syntax "Python Django"](https://bitbucket.org/StephaneBunel/pythonpep8autoformat/issue/10/does-not-work-with-syntax-python-django)

### 2013.03.24-01
  - ADD ST3 Compatibility. Tested with ST3 build 3021
  - ADD folder libs/py33 for Sublime Text 3 compatibility. ST3 use Python 3.3.0
  - ADD diff_match_patch.py in libs/py33 patched by Sergey Trupp.

### 2013.03.22-01
  - FIX Check syntax mode before trying to reformat
  - Upgrade pep8 to version 1.4.6a0
  - Upgrade autopep8 to version 0.8.7
  - Prepare plug-in for ST3 (Sublime Text 3). Thanks to Sergey Trupp
  - FIX Syntax error on sys.exc_info()

### 2013.02.07-01
  - Quick and Dirty FIX to [multiline-selection-on-saving-formatting][issue9]: reformatting on block selection is now disabled.
  Let me know if this feature was useful by opening a request to reintroduce it.

### 2013.01.26-02
  - Now use the module API exposed by autopep8.

### 2013.01.26-01
  - FIX [Issue #7 View jumping in Sublime][issue7]. Thanks to Timon Wong
  - Update autopep8 to version 0.8.5.

### 2012.12.31-01
  - Add Side bar menu

### 2012.12.20-01
  - FIX [Issue #8 Autoformat on save][issue8]. Thanks to zhenyu li
  - Update autopep8 to version 0.8.4.

### 2012.11.24-01
 - Update autopep8 to version 0.8.3.
 - Add new setting "aggressive" (default false) to enable possibly unsafe changes (E711, E712)

### 2012.11.02-1
 - Update autopep8 to version 0.8.2. This version fix [W601][fix-github-issue40]
 - Add new setting "max-line-length" (default is 79).

### 2012.10.14-1
- FIX: [Issue #4][issue4] (format only one round).
- Upgrade shipped autopep8.
- Remove "command" and "show_command" settings since pep8 and autopep8 are bundled with plug-in
- autopep8: Some strange behavior persists when code contains non-ASCII characters.
  Ex: [Fixing W601][fix-github-issue40] silently fails if source code contains non-ascii.

### 2012.09.29-1
- FIX: [Issue #3][issue3]. ST2 pep8 version issue on OS X.

### 2012.09.22-1
- Upgrade autopep8 utility to version 0.8.1.

### 2012.09.16-1
- FIX: Issue #2 raises another issue from autopep8 and Unicode, see [autopep8 issue #40][autopep8-issue40].
Now uses a fixed autopep8 from [hhatto/fix-github-issue40][fix-github-issue40] branch.
- Upgrade shipped lib2to3 to version 2.6.8

### 2012.09.14-1
- FIX: [Issue #2][issue2]. Include lib2to3 in package (Jim Wallace).

### 2012.09.10-1
- FIX: Enhance python code recognition

### 2012.09.07-1
- autopep8 updated to version 0.8

### 2012.08.02-1
- Python PEP8 Autoformat is now installable via [Sublime Package Control][spp]

### 2012.07.22-1
- Cosmetic code changes
- Tested under Windows 7
- Cleanup README.md (thanks to Georges Goncalves)

### 2012.07.21-2
- README update

### 2012.07.21-2
- FIX: OSX key binding is now `ctrl+shift+r`

### 2012.07.21-1
- NEW: Settings menu (Preferences -> Packages Settings -> Python PEP8 Autoformat)
- NEW: Include pep8 v1.3.3 and autopep8 v0.7.2 (with minor modifications) to remove all dependencies.
- FIX: variable reference

### 2012.07.19-1
- FIX: Restore caret position after reformatting the whole document.
- NEW: This file (CHANGELOG.md).

### 2012.07.18-1
- ADD: `Default.sublime-commands` file to call "Python PEP8 Autoformat" from Command Panel.
- ADD: `pep8_autoformat.sublime-settings` settings file:

		{
			// autoformat code on save ?
			"autoformat_on_save": false,

		    // path to autopep8 or "" to use packaged version
		    "command": "",

		    // select errors / warnings(e.g. ["E4", "W"])
		    "select": [],

		    // do not fix these errors / warnings(e.g. ["E4", "W"])
		    "ignore": ["E501"],

		    // for debugging purpose (print executed command in console)
		    "show_command": false
		}

### 2012.07.17-1
- Source code published on [bitbucket][sources].

### 2012.07.15-1
- Learning how to create a plug-in for Sublime text 2,
- and write the first lines of code.


[sources]: https://bitbucket.org/StephaneBunel/pythonpep8autoformat
[spp]: http://wbond.net/sublime_packages/package_control
[issue2]: https://bitbucket.org/StephaneBunel/pythonpep8autoformat/issue/2/import-error-during-formatting
[autopep8-issue40]: https://github.com/hhatto/autopep8/issues/40
[fix-github-issue40]: https://github.com/hhatto/autopep8/issues/40
[issue3]: https://bitbucket.org/StephaneBunel/pythonpep8autoformat/issue/3/downloaded-and-getting-error-on-ctrl-shift
[issue4]: https://bitbucket.org/StephaneBunel/pythonpep8autoformat/issue/4/format-only-one-round
[issue7]: https://bitbucket.org/StephaneBunel/pythonpep8autoformat/issue/7/view-jumping-in-sublime
[issue8]: https://bitbucket.org/StephaneBunel/pythonpep8autoformat/issue/8/change-on_post_save-to-on_pre_save
[issue9]: https://bitbucket.org/StephaneBunel/pythonpep8autoformat/issue/9/multiline-selection-on-saving-formatting
[issue15]: https://bitbucket.org/StephaneBunel/pythonpep8autoformat/issue/15/add-no-sublime-package
